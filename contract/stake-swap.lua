-- uniswap && ERC20
-- add this Contract type when only compile by gluac
type Contract<T> = {
    storage: T
}

-- type State = 'NOT_INITED' | 'COMMON' | 'PAUSED' | 'STOPPED'

let MAX_TOKEN_AMOUNT = 200000000000000000

type Storage = {
    name: string,
    symbol: string,
    supply: string, -- bigint string
    precision: int, -- only used to display
    state: string,
    allowLock: bool,
    fee: int, 
    minTransferAmount: int, 
    feeReceiveAddress: string, 
    admin: string, -- admin user address
    
    ------uniswap info
    fee_rate:string, 
    token_1_contractAddr:string,
    token_2_contractAddr:string,
    token_1_pool_amount:int, --bigint string
    token_2_pool_amount:int, --bigint string
    min_token1_amount:int,
    min_token2_amount:int,
    dao_contract: string 
}

-- events: Transfer, Paused, Resumed, Stopped, AllowedLock, Locked, Unlocked
var M = Contract<Storage>()

function M:init()
    print("uniswap contract creating")
    self.storage.name = ''
    self.storage.symbol = ''
    self.storage.supply = "0"
    self.storage.precision = 0
    self.storage.state = 'NOT_INITED'
    self.storage.admin = caller_address
    self.storage.allowLock = false
    self.storage.fee = 0
    self.storage.minTransferAmount = 0
    self.storage.feeReceiveAddress = caller_address
    
    self.storage.fee_rate = '0.0'
    self.storage.token_1_contractAddr = ''
    self.storage.token_2_contractAddr = ''
    self.storage.token_1_pool_amount = 0
    self.storage.token_2_pool_amount = 0
    self.storage.min_token1_amount = 0
    self.storage.min_token2_amount = 0
    self.storage.dao_contract = ''
    
    print("uniswap contract created")
end

let function getInputPrice(input_amount:int, input_reserve:int, output_reserve:int)
    let a = safemath.safenumber(input_reserve)
    let b = safemath.safenumber(output_reserve)
    let x = safemath.safenumber(input_amount)
    let n = safemath.number_multiply(x, b)
    let r = safemath.number_toint(safemath.number_div(n, safemath.number_add(a, x)))
    return r
end

let function get_from_address()
    var from_address: string
    let prev_contract_id = get_prev_call_frame_contract_address()
    if prev_contract_id and is_valid_contract_address(prev_contract_id) then
        from_address = prev_contract_id
    else
        from_address = caller_address
    end
	
    return from_address
end

let function checkAdmin(self: table)
    if self.storage.admin ~= get_from_address() then
        return error("you are not admin, can't call this function")
    end
end

-- parse a,b,c format string to [a,b,c]
let function parse_args(arg: string, count: int, error_msg: string)
    if not arg then
        return error(error_msg)
    end
	
    let parsed = string.split(arg, ',')
    if (not parsed) or (#parsed ~= count) then
        return error(error_msg)
    end
    return parsed
end

let function parse_at_least_args(arg: string, count: int, error_msg: string)
    if not arg then
        return error(error_msg)
    end
    
    let parsed = string.split(arg, ',')
    if (not parsed) or (#parsed < count) then
        return error(error_msg)
    end
    return parsed
end

let function arrayContains(col: Array<object>, item: object)
    if not item then
        return false
    end
    
    var value: object
    for _, value in ipairs(col) do
        if value == item then
            return true
        end
    end
    return false
end


let function checkState(self: table)
    if self.storage.state == 'NOT_INITED' then
        return error("contract not inited")
    end
    if self.storage.state == 'PAUSED' then
        return error("contract paused")
    end
    if self.storage.state == 'STOPPED' then
        return error("contract stopped")
    end
end

let function checkStateInited(self: table)
    if self.storage.state == 'NOT_INITED' then
        return error("contract token not inited")
    end
end

let function checkAddress(addr: string)
    let result = is_valid_address(addr)
    if not result then
        return error("address format error")
    end
    return result
end

let function is_native_asset_symbol(token_address: string)
    let len = #token_address
    if not token_address then
        return false
    end
	
    if len < tointeger(1) then
        return false
    end
	
    if tointeger(15) < len then
        return false
    end
	
    return true
end

let function changeUserNativeBalance(self: table, address: string, symbol: string, change: int)
    if change == 0 then
        return
    end
    
    let userAssets: table = json.loads(fast_map_get('nativeBalances', address) or '{}') or {}
    let oldBalance = tointeger(userAssets[symbol] or 0)
    let newBalance = 0
    if change < 0 then
        let amount = - change
        if oldBalance < amount then
            return error("amount exceed balance")
        end
        newBalance = oldBalance - amount
    else
        newBalance = oldBalance + change
    end
    
    userAssets[symbol] = newBalance
    let newUserAssetsStr = json.dumps(userAssets)
    fast_map_set('nativeBalances', address, newUserAssetsStr)
    let nativeTransferEventArg = json.dumps({
        address: address,
        symbol: symbol,
        change: change,
        reason: ''
    })
    emit NativeBalanceChange(nativeTransferEventArg)
end


let function withdraw_native_asset_private(self:table,from:string,symbol:string,amountStr:string)
    checkState(self)    

    let amount = tointeger(amountStr)

    if (not symbol) or (#symbol < 1) or (not amount) or (amount <= 0) then
        return error("invalid params")
    end
    
    let fromAddress = from
    let userAssets: table = json.loads(fast_map_get('nativeBalances', fromAddress) or '{}') or {}
    let oldBalance = tointeger(userAssets[symbol] or 0)
    if oldBalance < amount then
        return error("amount exceed balance")
    end
    
    let newBalance = oldBalance - amount
    userAssets[symbol] = newBalance
    let newUserAssetsStr = json.dumps(userAssets)

    fast_map_set('nativeBalances', fromAddress, newUserAssetsStr)
    let res1 = transfer_from_contract_to_address(fromAddress, symbol, amount)
    if res1 ~= 0 then
        return error("transfer asset " .. symbol .. " to " .. fromAddress .. " amount:"..tostring(amount).." error, error code: " .. tostring(res1))
    end 
    
    let nativeTransferEventArg = json.dumps({
        address: fromAddress,
        symbol: symbol,
        change: - amount,
        reason: 'withdraw'
    })
    emit NativeBalanceChange(nativeTransferEventArg)
end


let function exchangePrivate(self: table, from_address: string, arg: string)
    checkState(self)
    let parsed = parse_at_least_args(arg, 5, "argument format error, need format is sell_token_addr,sell_token_amount,want_buy_token_addr,min_want_buy_token_amount,expired_blocknum,withdraw(optional)")
    
    let sell_token_addr = tostring(parsed[1])
    var sell_token_amount:int = tointeger(parsed[2])
    let want_buy_token_addr = tostring(parsed[3])
    let want_buy_token_amount = tointeger(parsed[4])
    let expired_blocknum = tointeger(parsed[5])
    let withdraw:bool = false
    if #parsed == tointeger(6) then
        withdraw = toboolean(parsed[6])
    end

    if((sell_token_amount<=0)or(want_buy_token_amount<=0)) then
        return error("argument format error, sell_token_amount or min_want_buy_token_amount > 0")
    end
        
    if(expired_blocknum <= 0) then
        return error("expired_blocknum must >= 0")
    end
    
    if(expired_blocknum <= get_header_block_num()) then
        return error("expired_blocknum must > head_block_num")
    end
    
    var token_1_pool_amount = self.storage.token_1_pool_amount
    var token_2_pool_amount = self.storage.token_2_pool_amount
    
    if(token_1_pool_amount <= 0) or (token_2_pool_amount <= 0) then
        return error("pool is empty")
    end
    
    let input_sell_token_amount = sell_token_amount
    sell_token_amount = sell_token_amount
    
    let supply = self.storage.supply
    let feeRate = self.storage.fee_rate
    let nFeeRate = safemath.safenumber(feeRate)
    let temp = safemath.number_multiply(safemath.safenumber(sell_token_amount),nFeeRate)
    var fee:int = safemath.number_toint(temp)
    if (safemath.number_ne(safemath.safenumber(fee), temp)) then
        fee = fee + 1
    end
    
    if (fee <= 0) then
        fee = 1
    end
    
    if(sell_token_amount <= fee) then
        return error("can't get any")
    end
    
    let token_1_contractAddr = self.storage.token_1_contractAddr
    let token_2_contractAddr = self.storage.token_2_contractAddr
    let event_arg = {}
    var get_token_amount:int = 0
    if((sell_token_addr==token_1_contractAddr) and (want_buy_token_addr==token_2_contractAddr)) then
        if (want_buy_token_amount >= token_2_pool_amount) then
            return error("pool amount not enough")
        end
		
        get_token_amount = getInputPrice(sell_token_amount-fee, token_1_pool_amount, token_2_pool_amount)
        if (get_token_amount <= 0) then
            return error("get_token2_amount <= 0")
        end
        if (want_buy_token_amount > get_token_amount) then
            return error("can't get what you want amount")
        end
		
        token_1_pool_amount = token_1_pool_amount + sell_token_amount
        token_2_pool_amount = token_2_pool_amount - get_token_amount
        event_arg["buy_asset"] = token_2_contractAddr
        
    elseif((sell_token_addr==token_2_contractAddr) and (want_buy_token_addr==token_1_contractAddr)) then
        if (want_buy_token_amount >= token_1_pool_amount) then
            return error("pool amount not enough")
        end
		
        get_token_amount = getInputPrice(sell_token_amount-fee, token_2_pool_amount, token_1_pool_amount)
        if (get_token_amount <= 0) then
            return error("get_token1_amount <= 0")
        end
		
        if (want_buy_token_amount > get_token_amount) then
            return error("can't get what you want amount")
        end
        token_2_pool_amount = token_2_pool_amount + sell_token_amount
        token_1_pool_amount = token_1_pool_amount - get_token_amount
        event_arg["buy_asset"] = token_1_contractAddr
    else
        return error("token address not match for exchange")
    end
    
    if (token_1_pool_amount <= 0) or (token_2_pool_amount <= 0) then
        return error("caculate internal error")
    end
    	   
    --sub from_address balance
    if is_native_asset_symbol(sell_token_addr) then
        changeUserNativeBalance(self, from_address, sell_token_addr, - sell_token_amount)
    else
        let transfer_from_contract = import_contract_from_address(sell_token_addr)
        let cur_contract = get_current_contract_address()
        let prifixstr = from_address ..","..cur_contract..","
        transfer_from_contract:transferFrom(prifixstr..tostring(sell_token_amount))
    end
    
    -- transfer to from_address
    let buy_asset_symbol = tostring(event_arg["buy_asset"])
    if is_native_asset_symbol(buy_asset_symbol) then
        changeUserNativeBalance(self, from_address, buy_asset_symbol, get_token_amount)
        if withdraw  then
            withdraw_native_asset_private(self,from_address,buy_asset_symbol,tostring(get_token_amount))
        end
        
    else
        let to_contract = import_contract_from_address(tostring(event_arg["buy_asset"]))
        to_contract:transfer(from_address..","..tostring(get_token_amount))
    end
    
    --set
    self.storage.token_1_pool_amount = token_1_pool_amount
    self.storage.token_2_pool_amount = token_2_pool_amount
    
    event_arg["addr"] = from_address
    event_arg["fee"] = fee -- fee symbol ->sell_asset
    event_arg["sell_asset"] = sell_token_addr
    event_arg["sell_amount"] = sell_token_amount
    event_arg["buy_amount"] = get_token_amount
    emit Exchanged(json.dumps(event_arg))
    
    let get_token_amount_str = tostring(get_token_amount)
    return get_token_amount_str
end

-- call dao 
let function call_dao_contract(self: table,from: string,to: string,amount:string)
    -- Nothing
end

function M:on_deposit_asset(jsonstrArgs: string)
    -- return error("not support deposit")
    checkState(self)    
    let arg = json.loads(jsonstrArgs)
    let amount = tointeger(arg.num)
    let symbol = tostring(arg.symbol)
    let param = tostring(arg.param)
    if (not amount) or (amount < 0) then
         return error("deposit should greater than 0")
    end
	
    if (not symbol) or (#symbol < 1) then
         return error("on_deposit_asset arg wrong")
    end
    
    if (symbol ~= self.storage.token_1_contractAddr) and (symbol ~= self.storage.token_2_contractAddr) then
        return error("only support deposit exchange pair assets")
    end
    
    let fromAddress = get_from_address()
    let userAssets: table = json.loads(fast_map_get('nativeBalances', fromAddress) or '{}') or {}
    let oldBalance = tointeger(userAssets[symbol] or 0)
    let newBalance = oldBalance + amount
    userAssets[symbol] = newBalance
    let newUserAssetsStr = json.dumps(userAssets)
    fast_map_set('nativeBalances', fromAddress, newUserAssetsStr)

    let nativeTransferEventArg = json.dumps({
        address: fromAddress,
        symbol: symbol,
        change: amount,
        reason: 'deposit'
    })
    emit NativeBalanceChange(nativeTransferEventArg)
	
    if #param > 0 then
        let parsedParam = string.split(param, ',')
        if #parsedParam >= tointeger(5) then
            return exchangePrivate(self, fromAddress, param)
        end
    end
end

function M:withdraw_native_asset(arg: string)
    checkState(self)    
    let parsedArgs = parse_at_least_args(arg, 2)
    let symbol = tostring(parsedArgs[1])
    let amount = tointeger(parsedArgs[2])

    if (not symbol) or (#symbol < 1) or (not amount) or (amount <= 0) then
        return error("invalid params")
    end
	
    let fromAddress = get_from_address()
    let userAssets: table = json.loads(fast_map_get('nativeBalances', fromAddress) or '{}') or {}
    let oldBalance = tointeger(userAssets[symbol] or 0)
    if oldBalance < amount then
        return error("amount exceed balance")
    end
	
    let newBalance = oldBalance - amount
    userAssets[symbol] = newBalance
    let newUserAssetsStr = json.dumps(userAssets)
    fast_map_set('nativeBalances', fromAddress, newUserAssetsStr)
    let res1 = transfer_from_contract_to_address(fromAddress, symbol, amount)
    if res1 ~= 0 then
        return error("transfer asset " .. symbol .. " to " .. fromAddress .. " amount:"..tostring(amount).." error, error code: " .. tostring(res1))
    end 
    
    let nativeTransferEventArg = json.dumps({
        address: fromAddress,
        symbol: symbol,
        change: - amount,
        reason: 'withdraw'
    })
    emit NativeBalanceChange(nativeTransferEventArg)
end

offline function M:query_native_asset(address: string)
    if not is_valid_address(address) then
        return error("invalid address param format")
    end
	
    let userAssets: table = json.loads(fast_map_get('nativeBalances', address) or '{}') or {}
    return userAssets
end

-- arg: token1_addr,token2_addr,min_token1_amount,min_token2_amount,fee_rate,liquidity_token_name,liquidity_token_symbol
function M:init_config(arg: string)
    checkAdmin(self)
    if self.storage.state ~= 'NOT_INITED' then
        return error("this contract inited before")
    end
    let parsed = parse_args(arg, 7, "arg format error, need format: token1_addr,token2_addr,min_token1_amount,min_token2_amount,fee_rate,liquidity_token_name,liquidity_token_symbol")
    let info = {token1_addr: parsed[1],token2_addr: parsed[2],min_token1_amount: parsed[3],min_token2_amount: parsed[4],fee_rate: parsed[5], liquidity_token_name: parsed[6],liquidity_token_symbol: parsed[7]}
    
    let token_1_contractAddr = tostring(info.token1_addr)
    if not is_native_asset_symbol(token_1_contractAddr) then
        let tokenContr1 = import_contract_from_address(token_1_contractAddr)
        if not tokenContr1 or (not tokenContr1.transferFrom) then
            return error("token1_addr not token contract")
        end
    end
	
    let token_2_contractAddr = tostring(info.token2_addr)
    if (token_2_contractAddr == token_1_contractAddr) then
        return error("token_2_contractAddr and token_1_contractAddr is same")
    end
	
    if not is_native_asset_symbol(token_2_contractAddr) then
        print(token_2_contractAddr, ' is contract token')
        let tokenContr2 = import_contract_from_address(token_2_contractAddr)
        if not tokenContr2 or (not tokenContr2.transferFrom) then
            return error("token1_addr not token contract")
        end
    end
	
    let min_token1_amount = tointeger(info.min_token1_amount)
    let min_token2_amount = tointeger(info.min_token2_amount)    
    if((min_token1_amount<=0) or (min_token2_amount<=0)) then
        return error("argument error, min_token_amount to add liquidity must be positive integer")
    end
    
    let fee_rate = tostring(info.fee_rate)
    let safenumber_fee_rate = safemath.safenumber(fee_rate)
    if (safemath.number_lt(safenumber_fee_rate,safemath.safenumber(0)) or safemath.number_gte(safenumber_fee_rate,safemath.safenumber(1))) then
        return error("fee rate must be >=0 and < 1")
    end
    
    let liquidity_token_name = tostring(info.liquidity_token_name)
    if (#liquidity_token_name < 1) then
        return error("liquidity_token_name is empty")
    end
    
    let liquidity_token_symbol = tostring(info.liquidity_token_symbol)    
    if (#liquidity_token_symbol < 1) then
        return error("liquidity_token_symbol is empty")
    end

    self.storage.name = liquidity_token_name
    self.storage.symbol = liquidity_token_symbol
    self.storage.precision = 1
    self.storage.token_1_contractAddr = token_1_contractAddr
    self.storage.token_2_contractAddr = token_2_contractAddr
    
    self.storage.min_token1_amount = min_token1_amount
    self.storage.min_token2_amount = min_token2_amount

    self.storage.fee_rate = fee_rate
    self.storage.state = 'COMMON'

    info["state"] = 'COMMON'
    info["precision"] = 1
    pprint('info4:', info)
    let eventArgStr = json.dumps(info)
    
    emit Inited(eventArgStr)
end

offline function M:state(arg: string)
    return self.storage.state
end

offline function M:tokenName(arg: string)
    checkStateInited(self)
    return self.storage.name
end

offline function M:precision(_: string)
    checkStateInited(self)
    return self.storage.precision
end

offline function M:tokenSymbol(arg: string)
    checkStateInited(self)
    return self.storage.symbol
end

offline function M:admin(_: string)
    checkStateInited(self)
    return self.storage.admin
end

offline function M:totalSupply(arg: string)
    checkStateInited(self)
    return self.storage.supply
end

offline function M:isAllowLock(_: string)
    let resultStr = tostring(self.storage.allowLock)
    return resultStr
end

offline function M:fee(_: string)
    let feeStr = tostring(self.storage.fee)
    return feeStr
end

offline function M:minTransferAmount(_: string)
    let minTransferAmountStr = tostring(self.storage.minTransferAmount)
    return minTransferAmountStr
end

offline function M:feeReceiveAddress(_: string)
    return self.storage.feeReceiveAddress
end

function M:setFee(feeStr: string)
    checkAdmin(self)
    checkState(self)
    let fee = tointeger(feeStr)
    if (fee ~= 0) and ((not fee) or (fee < 0)) then
        return error("error fee format")
    end
	
    self.storage.fee = fee
    emit FeeChanged(feeStr)
end

function M:setMinTransferAmount(minTransferAmountStr: string)
    checkAdmin(self)
    checkState(self)
    let minTransferAmount = tointeger(minTransferAmountStr)
    if (minTransferAmount ~= 0) and ((not minTransferAmount) or (minTransferAmount < 0)) then
        return error("error minTransferAmount format")
    end
    self.storage.minTransferAmount = minTransferAmount
    emit MinTransferAmountChanged(minTransferAmountStr)
end

function M:setFeeReceiveAddress(feeReceiveAddress: string)
    checkAdmin(self)
    checkState(self)
    if not is_valid_address(feeReceiveAddress) then
        return error("invalid address")
    end
    if is_valid_contract_address(feeReceiveAddress) then
        return error("can't use contract address")
    end
    self.storage.feeReceiveAddress = feeReceiveAddress
    emit FeeReceiveAddressChanged(feeReceiveAddress)
end


function M:openAllowLock(_: string)
    checkAdmin(self)
    checkState(self)
    if self.storage.allowLock then
        return error("this contract had been opened allowLock before")
    end
    self.storage.allowLock = true
    emit AllowedLock("")
end

let function getBalanceOfUser(self: table, addr: string)
    return tostring(fast_map_get('users', addr) or 0)
end

offline function M:balanceOf(owner: string)
    checkStateInited(self)
    if (not owner) or (#owner < 1) then
        return error('arg error, need owner address as argument')
    end
	
    checkAddress(owner)
    let amount = getBalanceOfUser(self, owner)
    let amountStr = tostring(amount)
    return amountStr
end

-- arg: limit(1-based),offset(0-based)}
offline function M:users(arg: string)
    return error("not implemented, you can find users from contract transaction history")
end

-- arg: to_address,integer_amount[,memo]
function M:transfer(arg: string)
    checkState(self)
    let parsed = parse_at_least_args(arg, 2, "argument format error, need format is to_address,integer_amount[,memo]")
    let info = {to: parsed[1], amount: tostring(parsed[2])}
    let to = tostring(info.to)
    let amount = safemath.bigint(info.amount)
    var memo: string = nil
    if #parsed >= 3 then
    memo = tostring(parsed[3])
    end
	
    if (not to) or (#to < 1) then
        return error("to address format error")
    end
	
    let fee = self.storage.fee
    let minTransferAmount = self.storage.minTransferAmount
    let feeReceiveAddress = self.storage.feeReceiveAddress
    if (not amount) or (safemath.lt(amount , safemath.bigint(1))) then
        return error("amount format error")
    end
	
    if safemath.le(amount , safemath.bigint(fee)) then
        return error("amount not enough for fee")
    end
	
    if safemath.lt( amount , safemath.bigint(minTransferAmount)) then
        return error("only transfer amount >= " .. tostring(minTransferAmount) .. " will be accepted")
    end
	
    checkAddress(to)
    let from_address = get_from_address()
    var from_address_balance = safemath.bigint(fast_map_get('users', from_address) or 0)
    if (not from_address_balance) or (safemath.lt(from_address_balance , amount)) then
        return error("you have not enoungh amount to transfer out")
    end
	
    from_address_balance = safemath.sub(from_address_balance , amount)
    fast_map_set('users', from_address, safemath.tostring(from_address_balance))
    if safemath.eq(from_address_balance , safemath.bigint(0)) then
        fast_map_set('users', from_address, nil)
    end
	
    let to_balance = safemath.bigint(fast_map_get('users', to) or 0)
    if safemath.lt(safemath.add(to_balance , amount) ,safemath.bigint(0)) then
        return error("amount overflow")
    end

    fast_map_set('users', to, safemath.tostring(safemath.sub(safemath.add(to_balance , amount) , safemath.bigint(fee))))
    if fee > 0 then
        let feeReceiveAddressOldBalance = safemath.bigint(fast_map_get('users', feeReceiveAddress) or 0)
        if safemath.lt(safemath.add(feeReceiveAddressOldBalance ,safemath.bigint( fee)) , safemath.bigint(0)) then
            return error("amount overflow")
        end
		
        fast_map_set('users', feeReceiveAddress, safemath.tostring(safemath.add(feeReceiveAddressOldBalance ,safemath.bigint( fee))))
    end
	
    let eventArgStr = json.dumps({from: from_address, to: to, amount: safemath.tostring(safemath.sub(amount ,safemath.bigint(fee))), fee: fee, memo: memo})
    emit Transfer(eventArgStr)

    if is_valid_contract_address(to) then
		let multiOwnedContract = import_contract_from_address(to)
		let amountStr = tostring(amount - fee)
		
		if multiOwnedContract and (multiOwnedContract.on_deposit_contract_token) then
			multiOwnedContract:on_deposit_contract_token(amountStr)
		end
    end
end

-- arg format: fromAddress,toAddress,amount(with precision)
function M:transferFrom(arg: string)
    checkState(self)
    let parsed = parse_at_least_args(arg, 3, "argument format error, need format is fromAddress,toAddress,amount(with precision)")
    let fromAddress = tostring(parsed[1])
    let toAddress = tostring(parsed[2])
    let amount = safemath.bigint(parsed[3])
    var memo: string = nil
    if #parsed >= 4 then
        memo = tostring(parsed[4])
    end
	
    checkAddress(fromAddress)
    checkAddress(toAddress)
    if (not amount) or (safemath.le(amount ,safemath.bigint( 0))) then
        return error("amount must be positive integer")
    end
    
    let fee = self.storage.fee
    let minTransferAmount = self.storage.minTransferAmount
    let feeReceiveAddress = self.storage.feeReceiveAddress
    if safemath.le(amount , safemath.bigint(fee)) then
        return error("amount not enough for fee")
    end
	
    if safemath.lt( amount , safemath.bigint(minTransferAmount)) then
        return error("only transfer amount >= " .. tostring(minTransferAmount) .. " will be accepted")
    end

    let from_address_balance = safemath.bigint(fast_map_get('users', fromAddress) or 0)
    if (not from_address_balance) or (safemath.lt(from_address_balance , amount)) then
        return error("you have not enoungh amount to transfer out")
    end
    
    let allowedDataStr = fast_map_get('allowed', fromAddress)
    if (not allowedDataStr) then
        return error("not enough approved amount to withdraw from:"..fromAddress.."to:"..toAddress)
    end
	
    let allowedData: Map<string> = totable(json.loads(tostring(allowedDataStr)))
    let contractCaller = get_from_address()
    if (not allowedData) or (not allowedData[contractCaller]) then
        return error("not enough approved amount to withdraw from:"..fromAddress.."to:"..toAddress.."contractCaller: "..contractCaller)
    end
	
    let approvedAmount = safemath.bigint(allowedData[contractCaller])
    if (not approvedAmount) or (safemath.gt(amount,approvedAmount)) then
        return error("not enough approved amount to withdraw from:"..fromAddress.."to:"..toAddress.."contractCaller: "..contractCaller)
    end
	
    let toAddressOldBalance = safemath.bigint(fast_map_get('users', toAddress) or 0)
    if safemath.lt( safemath.add(toAddressOldBalance , amount) , safemath.bigint(0)) then
        return error("amount overflow")
    end
	
    fast_map_set('users', toAddress, safemath.tostring(safemath.sub(safemath.add(toAddressOldBalance , amount) , safemath.bigint(fee))))
    if fee > 0 then
        let feeReceiveAddressOldBalance = safemath.bigint(fast_map_get('users', feeReceiveAddress) or 0)
        if safemath.lt(safemath.add(feeReceiveAddressOldBalance ,safemath.bigint( fee)) , safemath.bigint(0)) then
            return error("amount overflow")
        end
		
        fast_map_set('users', feeReceiveAddress, safemath.tostring(safemath.add(feeReceiveAddressOldBalance ,safemath.bigint( fee))))
    end
	
    fast_map_set('users', fromAddress, safemath.tostring(safemath.sub(safemath.bigint(fast_map_get('users', fromAddress)) , amount)))
    if safemath.eq(safemath.bigint(fast_map_get('users', fromAddress)) , safemath.bigint(0)) then
        fast_map_set('users', fromAddress, nil)
    end
	
    allowedData[contractCaller] =safemath.tostring(safemath.sub( approvedAmount , amount))
    if allowedData[contractCaller] == 0 then
        allowedData[contractCaller] = nil
    end
	
    fast_map_set('allowed', fromAddress, json.dumps(allowedData))
    let eventArgStr = json.dumps({from: fromAddress, to: toAddress, amount: safemath.tostring(safemath.sub(amount ,safemath.bigint(fee))), fee: fee, memo: memo})
    emit Transfer(eventArgStr)
end

-- arg format: spenderAddress,amount(with precision)
function M:approve(arg: string)
    checkState(self)
    let parsed = parse_at_least_args(arg, 2, "argument format error, need format is spenderAddress,amount(with precision)")
    let spender = tostring(parsed[1])
    checkAddress(spender)
    let amount = safemath.bigint(parsed[2])
    if (not amount) or (safemath.lt(amount , safemath.bigint(0))) then
        return error("amount must be non-negative integer")
    end
	
    var allowedData: Map<string>
    let contractCaller = get_from_address()
    if (not fast_map_get('allowed', contractCaller)) then
        allowedData = {}
    else
        allowedData = totable(json.loads(tostring(fast_map_get('allowed', contractCaller))))
        if not allowedData then
            return error("allowed storage data error")
        end
    end
	
    allowedData[spender] = safemath.tostring(amount)
    fast_map_set('allowed', contractCaller, json.dumps(allowedData))
    let eventArg = {from: contractCaller, spender: spender, amount: amount}
    emit Approved(json.dumps(eventArg))
end

-- arg format: spenderAddress,authorizerAddress
offline function M:approvedBalanceFrom(arg: string)
    let parsed = parse_at_least_args(arg, 2, "argument format error, need format is spenderAddress,authorizerAddress")
    let spender = tostring(parsed[1])
    let authorizer = tostring(parsed[2])
    checkAddress(spender)
    checkAddress(authorizer)
    let allowedDataStr = fast_map_get('allowed', authorizer)
    if (not allowedDataStr) then
        return "0"
    end
	
    let allowedData: Map<string> = totable(json.loads(tostring(allowedDataStr)))
    if (not allowedData) then
        return "0"
    end
	
    let allowedAmount = allowedData[spender]
    if (not allowedAmount) then
        return "0"
    end
	
    let allowedAmountStr = tostring(allowedAmount)
    return allowedAmountStr
end

-- arg format: fromAddress
offline function M:allApprovedFromUser(arg: string)
    let authorizer = arg
    checkAddress(authorizer)
    let allowedDataStr = fast_map_get('allowed', authorizer)
    if (not allowedDataStr) then
        return "{}"
    end
	
    return allowedDataStr
end

function M:pause(arg: string)
    if self.storage.state == 'STOPPED' then
        return error("this contract stopped now, can't pause")
    end
	
    if self.storage.state == 'PAUSED' then
        return error("this contract paused now, can't pause")
    end
	
    checkAdmin(self)
    self.storage.state = 'PAUSED'
    emit Paused("")
end

function M:resume(arg: string)
    if self.storage.state ~= 'PAUSED' then
        return error("this contract not paused now, can't resume")
    end
	
    checkAdmin(self)
    self.storage.state = 'COMMON'
    emit Resumed("")
end

function M:stop(arg: string)
    if self.storage.state == 'STOPPED' then
        return error("this contract stopped now, can't stop")
    end
	
    if self.storage.state == 'PAUSED' then
        return error("this contract paused now, can't stop")
    end
	
    checkAdmin(self)
    self.storage.state = 'STOPPED'
    emit Stopped("")
end

-- arg: integer_amount,unlockBlockNumber
function M:lock(arg: string)
    checkState(self)
    if (not self.storage.allowLock) then
        return error("this token contract not allow lock balance")
    end
	
    let parsed = parse_args(arg, 2, "arg format error, need format is integer_amount,unlockBlockNumber")
    let toLockAmount = safemath.bigint(parsed[1])
    let unlockBlockNumber = tointeger(parsed[2])
    if (not toLockAmount) or (safemath.lt(toLockAmount,safemath.bigint(1))) then
        return error("to unlock amount must be positive integer")
    end
	
    if (not unlockBlockNumber) or (unlockBlockNumber < get_header_block_num()) then
        return error("to unlock block number can't be earlier than current block number " .. tostring(get_header_block_num()))
    end
	
    let from_address = get_from_address()
    if from_address ~= caller_address then
        return error("only common user account can lock balance") -- 只有普通账户可以锁仓，合约不能锁仓
    end
	
    let balance = safemath.bigint(getBalanceOfUser(self, from_address))
    if safemath.gt(toLockAmount , balance) then
        return error("you have not enough balance to lock")
    end
	
    if (not fast_map_get('lockedAmounts', from_address)) then
        fast_map_set('lockedAmounts', from_address, safemath.tostring(toLockAmount) .. ',' .. tostring(unlockBlockNumber))
    else
        return error("you have locked balance now, before lock again, you need unlock them or use other address to lock")
    end
	
    fast_map_set('users', from_address, safemath.tostring(safemath.sub(balance , toLockAmount)))
    emit Locked(safemath.tostring(toLockAmount))
end

function M:unlock(_: string)
    checkState(self)
    if (not self.storage.allowLock) then
        return error("this token contract not allow lock balance")
    end
	
    let from_address = get_from_address()
    if (not fast_map_get('lockedAmounts', from_address)) then
        return error("you have not locked balance")
    end
	
    let lockedInfoParsed = parse_args(tostring(fast_map_get('lockedAmounts', from_address)), 2, "locked amount info format error")
    let lockedAmount = safemath.bigint(lockedInfoParsed[1])
    let canUnlockBlockNumber = tointeger(lockedInfoParsed[2])
    if (get_header_block_num() < canUnlockBlockNumber) then
        return error("your locked balance only can be unlock after block #" .. tostring(canUnlockBlockNumber))
    end
	
    fast_map_set('lockedAmounts', from_address, nil)
    let fromAddressOldBalance =safemath.bigint( getBalanceOfUser(self, from_address))
    if safemath.lt(safemath.add(fromAddressOldBalance,lockedAmount) , safemath.bigint(0)) then
        return error("amount overflow")
    end
	
    fast_map_set('users', from_address, safemath.tostring(safemath.add(fromAddressOldBalance,lockedAmount)))
    emit Unlocked(from_address .. ',' .. safemath.tostring(lockedAmount))
end

function M:forceUnlock(arg: string)
    checkState(self)
    if (not self.storage.allowLock) then
        return error("this token contract not allow lock balance")
    end
	
    checkAdmin(self)
    let userAddr = arg
    if (not userAddr) or (#userAddr < 1) then
        return error("argument format error, need format userAddress")
    end
	
    checkAddress(userAddr)
    if (not fast_map_get('lockedAmounts', userAddr)) then
        return error("this user have not locked balance")
    end
	
    let lockedInfoParsed = parse_args(tostring(fast_map_get('lockedAmounts', userAddr)), 2, "locked amount info format error")
    let lockedAmount = safemath.bigint(lockedInfoParsed[1])
    let canUnlockBlockNumber = tointeger(lockedInfoParsed[2])

    if (get_header_block_num() < canUnlockBlockNumber) then
        return error("this user locked balance only can be unlock after block #" .. tostring(canUnlockBlockNumber))
    end
	
    fast_map_set('lockedAmounts', userAddr, nil)
    let userOldBalance =safemath.bigint( getBalanceOfUser(self, userAddr))
    if safemath.lt(safemath.add(userOldBalance , lockedAmount) , safemath.bigint(0)) then
        return error("amount overflow")
    end
	
    fast_map_set('users', userAddr, safemath.tostring(safemath.add(userOldBalance , lockedAmount)))
    emit Unlocked(userAddr .. ',' .. safemath.tostring(lockedAmount))
end

offline function M:lockedBalanceOf(owner: string)
    if (not fast_map_get('lockedAmounts', owner)) then
        return '0,0'
    else
        let resultStr = fast_map_get('lockedAmounts', owner)
        return resultStr
    end
end

----------------------uniswap-------------------------------------
function M:addLiquidity(arg: string)
    checkState(self)
    let parsed = parse_at_least_args(arg, 3, "argument format error, need format is add_token1_amount,max_add_token2_amount,expired_blocknum")
    let add_token1_amount = tointeger(parsed[1])
    let max_add_token2_amount = tointeger(parsed[2])
    let expired_blocknum = tointeger(parsed[3])

    if((add_token1_amount<=0)or(max_add_token2_amount<=0)) then
        return error("add token amount must > 0")
    end     
	
    if(expired_blocknum <= 0) then
        return error("expired_blocknum must >= 0")
    end
	
    if(add_token1_amount<(self.storage.min_token1_amount)) then
        return error("add_token1_amount must >= min_token1_amount")
    end
	
    if(max_add_token2_amount<(self.storage.min_token2_amount)) then
        return error("max_add_token2_amount must >= min_token2_amount")
    end 
	
    if(expired_blocknum <= get_header_block_num()) then
        return error("expired_blocknum must > head_block_num")
    end 
	
    if((add_token1_amount > MAX_TOKEN_AMOUNT) or (max_add_token2_amount > MAX_TOKEN_AMOUNT)) then
        return error("add_token_amoun must <= "..tostring(MAX_TOKEN_AMOUNT))
    end

    let token_1_pool_amount = self.storage.token_1_pool_amount
    let token_2_pool_amount = self.storage.token_2_pool_amount    
    if((token_1_pool_amount > MAX_TOKEN_AMOUNT) or (token_2_pool_amount > MAX_TOKEN_AMOUNT)) then
        return error("token_pool_amount exceed MAX_TOKEN_AMOUNT:"..tostring(MAX_TOKEN_AMOUNT))
    end

    let add_token1_amount_safenumber = safemath.safenumber(add_token1_amount)
    var caculate_token2_amount:int = max_add_token2_amount

    let safenumber_0 = safemath.safenumber(0)
    if((token_1_pool_amount~=0) and (token_2_pool_amount~=0)) then
        let temp = safemath.number_div(safemath.number_multiply(add_token1_amount_safenumber,token_2_pool_amount),token_1_pool_amount)
        caculate_token2_amount = tointeger(safemath.number_toint(temp))
        if(safemath.number_ne(safemath.safenumber(caculate_token2_amount),temp)) then
            caculate_token2_amount = caculate_token2_amount + 1
        end
		
        if(caculate_token2_amount > max_add_token2_amount) then
            return error("caculate_token2_amount > max_add_token2_amount")
        end
    end

    if((MAX_TOKEN_AMOUNT-token_1_pool_amount)< add_token1_amount) then
        return error("after add, token_1_pool_amount exceed MAX_TOKEN_AMOUNT:"..tostring(MAX_TOKEN_AMOUNT))
    end
    
    if((MAX_TOKEN_AMOUNT-token_2_pool_amount)< caculate_token2_amount) then
        return error("after add, token_2_pool_amount exceed MAX_TOKEN_AMOUNT:"..tostring(MAX_TOKEN_AMOUNT))
    end
    
    let from_address = get_from_address()
    let cur_contract = get_current_contract_address()
    let prifixstr = from_address ..","..cur_contract..","
    let token_1_contractAddr = self.storage.token_1_contractAddr
    let token_2_contractAddr = self.storage.token_2_contractAddr

    if is_native_asset_symbol(token_1_contractAddr) then
        changeUserNativeBalance(self, from_address, token_1_contractAddr, - add_token1_amount)
    else
        let token1_contract = import_contract_from_address(token_1_contractAddr)
        token1_contract:transferFrom(prifixstr..tostring(add_token1_amount))
    end

    if is_native_asset_symbol(token_2_contractAddr) then
        changeUserNativeBalance(self, from_address, token_2_contractAddr, - caculate_token2_amount)
    else
        let token2_contract = import_contract_from_address(token_2_contractAddr)
        token2_contract:transferFrom(prifixstr..tostring(caculate_token2_amount))
    end

    let token_amount = safemath.bigint(0)
    let supply = safemath.bigint(self.storage.supply)
    if((token_1_pool_amount==0) and (token_2_pool_amount==0)) then
        token_amount = safemath.mul(safemath.bigint(add_token1_amount),safemath.bigint(1000))
    elseif((token_1_pool_amount~=0) and (token_2_pool_amount~=0)) then
        token_amount = safemath.div(safemath.mul(safemath.bigint(safemath.number_toint(add_token1_amount_safenumber)), supply), safemath.bigint(token_1_pool_amount))
    else
        return error("internal error, token_1_pool_amount,token_2_pool_amount,supply not unified 0")
    end

    if(safemath.le(token_amount,safemath.bigint(0) )) then
        return error("get liquidity token amount is 0")
    end
    
    let supply_new = safemath.add(supply, token_amount)
    if(safemath.ne(safemath.sub(supply_new ,supply) , token_amount) )then
        return error("supply overflow")
    end

    self.storage.token_1_pool_amount = token_1_pool_amount + add_token1_amount
    self.storage.token_2_pool_amount = token_2_pool_amount + caculate_token2_amount
    self.storage.supply = safemath.tostring(supply_new)
    
    let bal = safemath.bigint(fast_map_get("users",from_address) or 0)
    let bal_new = safemath.add(bal , token_amount)
    fast_map_set("users",from_address,safemath.tostring(bal_new))

    let eventArg = {}
    eventArg[token_1_contractAddr] = add_token1_amount
    eventArg[token_2_contractAddr] = caculate_token2_amount
    emit LiquidityAdded(json.dumps(eventArg))
    emit LiquidityTokenMinted(safemath.tostring(token_amount))
    let eventArgStr = json.dumps({from: "", to: from_address, amount: safemath.tostring(token_amount), fee: "0", memo: ""})
    emit Transfer(eventArgStr)
end

function M:removeLiquidity(arg: string)
    let parsed = parse_at_least_args(arg, 4, "argument format error, need format is destory_liquidity_token_amount,min_remove_asset1_amount,min_remove_asset2_amount,expired_blocknum,withdraw(optional)")
    
    let destory_liquidity_token_amount = safemath.bigint(parsed[1])
    let min_remove_token1_amount = tointeger(parsed[2])
    let min_remove_token2_amount = tointeger(parsed[3])
    let expired_blocknum = tointeger(parsed[4])
    let withdraw = false
    if #parsed > tointeger(4) then
        withdraw = toboolean(parsed[5])
    end

    if((min_remove_token1_amount<0)or(min_remove_token2_amount<0)) then
        return error("argument format error, input remove token amount must >= 0")
    end
        
    if(expired_blocknum <= 0) then
        return error("expired_blocknum must >= 0")
    end
    
    if(expired_blocknum <= get_header_block_num()) then
        return error("expired_blocknum must > head_block_num")
    end
    
    let from_address = get_from_address()
    let bal = safemath.bigint(fast_map_get("users", from_address) or 0)
    if safemath.gt(destory_liquidity_token_amount , bal) then
        return error("you have not enough liquidity to remove")
    end
    
    let token_1_pool_amount = self.storage.token_1_pool_amount
    let token_2_pool_amount = self.storage.token_2_pool_amount
    let supply = safemath.bigint(self.storage.supply)
    
    if ((token_1_pool_amount <= 0) or (safemath.le(supply,safemath.bigint(0))) or (token_2_pool_amount<= 0)) then
        return error("pool is empty")
    end
	
    if ((min_remove_token1_amount > token_1_pool_amount) or (min_remove_token2_amount > token_2_pool_amount)) then
        return error("wrong remove_token_amount")
    end
    
    var caculate_token1_amount:int = 0
    var caculate_token2_amount:int = 0
    
    if (safemath.eq(supply ,destory_liquidity_token_amount)) then
        caculate_token1_amount = token_1_pool_amount
        caculate_token2_amount = token_2_pool_amount
    else
        let tk = destory_liquidity_token_amount
        caculate_token1_amount = safemath.toint(safemath.div(safemath.mul(tk, safemath.bigint(token_1_pool_amount)),supply))
        caculate_token2_amount = safemath.toint(safemath.div(safemath.mul(tk, safemath.bigint(token_2_pool_amount)),supply))    
    end
    
    if (caculate_token1_amount < min_remove_token1_amount) then
        return error("caculate_token1_amount < min_remove_token1_amount")
    end
	
    if (caculate_token2_amount < min_remove_token2_amount) then
        return error("caculate_token2_amount < min_remove_token2_amount")
    end
    
    let token_1_contractAddr = self.storage.token_1_contractAddr
    let token_2_contractAddr = self.storage.token_2_contractAddr    
    let prifixstr = from_address..","
    
    if is_native_asset_symbol(token_1_contractAddr) then
        changeUserNativeBalance(self, from_address, token_1_contractAddr, caculate_token1_amount)
        if withdraw  then
            withdraw_native_asset_private(self,from_address,token_1_contractAddr,tostring(caculate_token1_amount))
        end
    else
        let token1_contract = import_contract_from_address(token_1_contractAddr)
        token1_contract:transfer(prifixstr..tostring(caculate_token1_amount))
    end
    
    if is_native_asset_symbol(token_2_contractAddr) then
        changeUserNativeBalance(self, from_address, token_2_contractAddr, caculate_token2_amount)
        if withdraw  then
            withdraw_native_asset_private(self,from_address,token_2_contractAddr,tostring(caculate_token2_amount))
        end 
    else
        let token2_contract = import_contract_from_address(token_2_contractAddr)
        token2_contract:transfer(prifixstr..tostring(caculate_token2_amount))
    end
    
    self.storage.token_1_pool_amount = token_1_pool_amount - caculate_token1_amount
    self.storage.token_2_pool_amount = token_2_pool_amount - caculate_token2_amount
    self.storage.supply =   safemath.tostring( safemath.sub( supply , destory_liquidity_token_amount))
    fast_map_set("users", from_address, safemath.tostring( safemath.sub(   bal,destory_liquidity_token_amount)))
    
    let eventArg = {}
    eventArg[token_1_contractAddr] = caculate_token1_amount
    eventArg[token_2_contractAddr] = caculate_token2_amount
    emit LiquidityRemoved(json.dumps(eventArg))
    emit LiquidityTokenDestoryed(safemath.tostring(destory_liquidity_token_amount)) 
    
    let transferEventArgStr = json.dumps({from: from_address, to: "", amount: safemath.tostring(destory_liquidity_token_amount), fee: "0", memo: ""})
    emit Transfer(transferEventArgStr)
end

--args: sell_token_addr,sell_token_amount,want_buy_token_addr,min_want_buy_token_amount,expired_blocknum
function M:exchange(arg: string)
    let from_address = get_from_address()
    return exchangePrivate(self, from_address, arg)
end

--arg: min_token1_amount,min_token2_amount 
function M:setMinAddAmount(arg: string)
    checkState(self)
    checkAdmin(self)
    
    let parsed = parse_at_least_args(arg, 2, "argument format error, need format is min_token1_amount,min_token2_amount")
    
    let min_token1_amount = tointeger(parsed[1])
    let min_token2_amount = tointeger(parsed[2])
    
    if(min_token1_amount<=0) or (min_token2_amount<=0) then
        return error("min_amount must > 0")
    end
    
    self.storage.min_token1_amount = min_token1_amount
    self.storage.min_token2_amount = min_token2_amount    
    emit SetMinAddAmount(arg)    
end

-------------------------------
offline function M:getInfo(arg: string)
    let infos = {}
    infos["admin"] = self.storage.admin
    infos["state"] = self.storage.state
    infos["fee_rate"] = self.storage.fee_rate
    infos["token_name"] = self.storage.name
    infos["token_symbol"] = self.storage.symbol
    infos["token_1_contractAddr"] = self.storage.token_1_contractAddr
    infos["token_2_contractAddr"] = self.storage.token_2_contractAddr
    infos["token_1_pool_amount"] = self.storage.token_1_pool_amount
    infos["token_2_pool_amount"] = self.storage.token_2_pool_amount
    infos["min_token1_amount"] = self.storage.min_token1_amount
    infos["min_token2_amount"] = self.storage.min_token2_amount
    infos["contract_addr"] = get_current_contract_address()
    let r = json.dumps(infos)
    return r
end

--arg: want_sell_token_addr,want_sell_token_amount,want_buy_token_addr
offline function M:caculateExchangeAmount(arg: string)
    checkState(self)
    let parsed = parse_at_least_args(arg, 3, "argument format error, need format is want_sell_token_addr,want_sell_token_amount,want_buy_token_addr")
    
    let want_sell_token_addr = tostring(parsed[1])
    var want_sell_token_amount:int = tointeger(parsed[2])
    let want_buy_token_addr = tostring(parsed[3])
    
    if (want_sell_token_amount<= 0) then
        return error("want_sell_token_amount must > 0")
    end
    
    let token_1_contractAddr = self.storage.token_1_contractAddr
    let token_2_contractAddr = self.storage.token_2_contractAddr
    
    let token_1_pool_amount = self.storage.token_1_pool_amount
    let token_2_pool_amount = self.storage.token_2_pool_amount
    
    if(token_1_pool_amount<=0) or (token_2_pool_amount<=0) then
        return error("pool is empty")
    end
    
    if(want_buy_token_addr == want_sell_token_addr) then
        return error("want_sell_token_addr and want_buy_token_addr is same")
    end
    
    if(want_sell_token_addr~=token_1_contractAddr) and (want_sell_token_addr~=token_2_contractAddr) then
        return error("want_sell_token_addr not match")
    end
    
    if(want_buy_token_addr~=token_1_contractAddr) and (want_buy_token_addr~=token_2_contractAddr) then
        return error("want_buy_token_addr not match")
    end
    
    want_sell_token_amount = want_sell_token_amount    
    let fee_rate = self.storage.fee_rate
    let nFeeRate = safemath.safenumber(fee_rate)
    let temp = safemath.number_multiply(safemath.safenumber(want_sell_token_amount),nFeeRate)
    var fee:int = safemath.number_toint(temp)
    if(safemath.number_ne(safemath.safenumber(fee),temp)) then
        fee = fee+1
    end
    
    if(fee<=0) then
        fee=1
    end
    
    if(want_sell_token_amount <= fee) then
        return "0"
    end
    
    var get_token_amount:int = 0
    if(want_sell_token_addr==token_1_contractAddr) and (want_buy_token_addr==token_2_contractAddr) then
        get_token_amount = getInputPrice(want_sell_token_amount-fee, token_1_pool_amount, token_2_pool_amount)
    elseif(want_sell_token_addr==token_2_contractAddr) and (want_buy_token_addr==token_1_contractAddr) then
        get_token_amount = getInputPrice(want_sell_token_amount-fee, token_2_pool_amount, token_1_pool_amount)
    else
        return error("input token address not match")
    end
    
    let get_token_amount_str = tostring(get_token_amount)
    return get_token_amount_str
end

--arg: liquidity_tokenAmount
offline function M:caculatePoolShareByToken(arg: string)
    checkState(self)
    let tokenAmount = safemath.bigint(arg)
    
    if (tokenAmount <= 0) then
        return error("input arg must be positive integer");
    end
    let supply = safemath.bigint(self.storage.supply)
    
    if safemath.gt(tokenAmount , supply) then
        return error("input tokenAmount must <= supply");
    end
    
    let token_1_contractAddr = self.storage.token_1_contractAddr
    let token_2_contractAddr = self.storage.token_2_contractAddr
    
    let token_1_pool_amount = self.storage.token_1_pool_amount
    let token_2_pool_amount = self.storage.token_2_pool_amount
    
    let result = {}
    if safemath.eq(tokenAmount , supply) then
        result[token_1_contractAddr] = token_1_pool_amount
        result[token_2_contractAddr] = token_2_pool_amount
    else
        result[token_1_contractAddr] =  safemath.toint(safemath.div(safemath.mul(tokenAmount, safemath.bigint(token_1_pool_amount)),supply))
        result[token_2_contractAddr] = safemath.toint(safemath.div(safemath.mul(tokenAmount, safemath.bigint(token_2_pool_amount)),supply)) 
    end
    
    let r = json.dumps(result)
    return r
end

--arg: address
offline function M:getUserRemoveableLiquidity(arg: string)
    checkState(self)
    let addr = arg
    let bal = fast_map_get("users",addr)
    
    let result = {}
    let token_1_contractAddr = self.storage.token_1_contractAddr
    let token_2_contractAddr = self.storage.token_2_contractAddr
    if not bal then
        result[token_1_contractAddr] = 0
        result[token_2_contractAddr] = 0
    else
        let token_balance = safemath.bigint(bal)
        let token_1_pool_amount = self.storage.token_1_pool_amount
        let token_2_pool_amount = self.storage.token_2_pool_amount
        let supply = safemath.bigint(self.storage.supply)
        
        if safemath.eq(token_balance , supply)  then
            result[token_1_contractAddr] = token_1_pool_amount
            result[token_2_contractAddr] = token_2_pool_amount
        else            
            result[token_1_contractAddr] = safemath.toint(safemath.div(safemath.mul(token_balance, safemath.bigint(token_1_pool_amount)),supply))
            result[token_2_contractAddr] = safemath.toint(safemath.div(safemath.mul(token_balance, safemath.bigint(token_2_pool_amount)),supply))   
        end     
    end
    
    let r = json.dumps(result)
    return r
end

function M:setDaoContractAddress(owner:string)
    checkState(self)
    checkAdmin(self)
    self.storage.dao_contract = owner
end

offline function M:getDaoContract(_:string)
    checkState(self)
    return self.storage.dao_contract
end

function M:on_destroy()
    error("can't destroy token contract")
end

return M
