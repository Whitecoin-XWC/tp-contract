-- add this Contract type when only compile by gluac
type Contract<T> = {
    storage: T
}

type Storage = {
    name: string,
    symbol: string,
    state: string,

    admin: string, -- admin user address
    stakingRewardStartBlock: Map<int>, 
    StakingRewardConfig:Map<string>, 
    StakingRewardLimit:Map<int>, 
    StakingRewardSupply:Map<int>, 
    allTokenSymbol:Map<string>, 

    allTokenUserCount: Map<int>, 
    allSymbolTotalBalance: Map<string>, 
    tokenAddress: string,
    isMigration: bool,
    totalSupplyLimit: int
}

-- events: Transfer, Paused, Resumed, Stopped, AllowedLock, Locked, Unlocked
var M = Contract<Storage>()

function M:init()
    print("stakeHome contract creating")
    self.storage.name = ''
    self.storage.symbol = ''
    self.storage.state = 'NOT_INITED'
    self.storage.admin = caller_address
    self.storage.allTokenSymbol = {}
    self.storage.allTokenUserCount = {}
    self.storage.allSymbolTotalBalance = {}
    self.storage.tokenAddress = ''
    self.storage.stakingRewardStartBlock = {}
    self.storage.isMigration = false
    self.storage.StakingRewardConfig = {}
    self.storage.StakingRewardLimit = {}
    self.storage.StakingRewardSupply = {}
    self.storage.totalSupplyLimit = 0
    print("stakeHome contract created")
end

let function checkState(self: table)
    if self.storage.state == 'NOT_INITED' then
        return error("contract token not inited")
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

let function checkMigration(self:table)
    if self.storage.isMigration then 
        return error("contract already migration!can't obtain reward ,can't deposit asset.only can withdraw asset!can't migration again!")
    end
end

let function check_caller_frame_valid(M: table)
    return
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

let function withdraw_native_asset_private(self:table,from:string,symbol:string,amountStr:string)
    checkState(self)	

    let amount = tointeger(amountStr)
    if (not symbol) or (#symbol < 1) or (not amount) or (amount <= 0) then
        return error("invalid params")
    end
	
    let fromAddress = from
    let oldBalance = tointeger(fast_map_get(symbol..'userBalance', fromAddress) or 0)
    if oldBalance < amount then
        return error("amount exceed balance")
    end
	
    let newBalance = oldBalance - amount
    fast_map_set(symbol..'userBalance', fromAddress, newBalance)
    self.storage.allSymbolTotalBalance[symbol] = safemath.tostring(safemath.sub( safemath.bigint( self.storage.allSymbolTotalBalance[symbol]) , safemath.bigint(amount)))
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

let function cal_user_can_reward(self:table,contract_addr:string , from_address:string,start_block:int)
    let token_symbol = self.storage.allTokenSymbol[contract_addr]
    if not token_symbol then 
        return error("The contract address is not currently supported for mining!")
    end
	
    let balanceTable = token_symbol.."userBalance"
    let addressTable = token_symbol.."userAddress"
    let lastRewardTable = token_symbol.."userRewardBlockNum"
    let staking_balance = safemath.bigint(fast_map_get(balanceTable,from_address) or 0 )
    if safemath.le(staking_balance ,  safemath.bigint(0)) then
        return  tointeger(0)
    end

    let all_staking_balance = safemath.bigint(self.storage.allSymbolTotalBalance[contract_addr])  
    let reward_supply = self.storage.StakingRewardSupply[contract_addr]
    let reward_limit = self.storage.StakingRewardLimit[contract_addr]
    if reward_supply>=reward_limit then
        return 0
    end
	
    let lastRewardBlock =start_block
    var i : int
    let rewardList = json.loads(self.storage.StakingRewardConfig[contract_addr])
    let range = (#rewardList)/2
    let lastPayBlockHeight:int = tointeger(fast_map_get(lastRewardTable,from_address))
    if (lastPayBlockHeight > tointeger(rewardList[range*2 -1])) then
        return tointeger(0)
    end

    let price = safemath.bigint(0)
    let current_block = tointeger(get_header_block_num())
    for i=0,range-1,1 do 
        if lastPayBlockHeight >= rewardList[i*2+1]  then
            if(i < range-1) then
                if (lastPayBlockHeight < rewardList[i*2+3]) then
                    let delta_block = math.min(current_block,rewardList[i*2+3]) - lastPayBlockHeight 
                    if(delta_block <0) then
                        return 0
                    end
					
                    price = safemath.add(price , safemath.mul(safemath.bigint(delta_block),safemath.bigint(rewardList[i*2+2])))
                    lastPayBlockHeight = math.min(current_block,rewardList[i*2+3])
                end
            end
        end
    end

    if current_block > tointeger(rewardList[range*2-1]) then
        if tointeger(rewardList[range*2]) > tointeger(0) then
            let delta_block = current_block - lastPayBlockHeight 
            if delta_block >tointeger(0) then
                price = safemath.add(price , safemath.mul(safemath.bigint(delta_block),safemath.bigint(rewardList[range*2])))
                lastPayBlockHeight = current_block
            end            
        end
    end

    let reward = safemath.div( safemath.mul(price,staking_balance),all_staking_balance)
    let reward_delta = (tointeger(reward_limit) - tointeger(reward_supply))
    if reward_delta < tointeger(0) then
        reward_delta = 0
    end
	
    let last_reward = math.min(safemath.toint(reward),reward_delta)
    return last_reward
end




-- 充值原生资产到本合约
function M:on_deposit_asset(jsonstrArgs: string)
    -- return error("not support deposit")
    checkState(self)	
    checkMigration(self)
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
    -- 只能支持交易对中的原生资产
    let token_symbol = self.storage.allTokenSymbol[symbol]
    if token_symbol == nil then
        return error("This asset is not supported. Please contact your administrator.")
    end
   
    let from_address = get_from_address()

    -- check user exist
    -- SymboluserBalance   fastMap  addr->balance
    -- SymbolUserAddress   fastMap  id -> addr
    let balanceTable = token_symbol.."userBalance"
    let addressTable = token_symbol.."userAddress"
    let lastRewardTable = token_symbol.."userRewardBlockNum"
    let user_originBalance = fast_map_get(balanceTable,from_address) 

    let user_origin_balance = safemath.bigint(0)
    let stakingCoinAddr = symbol
    if user_originBalance == nil  then
        -- user not exsit
        -- add user 
        let symbolUserCount = tointeger(self.storage.allTokenUserCount[stakingCoinAddr])
        
        self.storage.allTokenUserCount[stakingCoinAddr] =  symbolUserCount + tointeger(1)
        fast_map_set(addressTable,tostring(symbolUserCount),from_address)
        let current_block = get_header_block_num()
        let startRewardBlockNumber = math.max(tointeger(current_block),tointeger(self.storage.stakingRewardStartBlock[stakingCoinAddr]))
        fast_map_set(lastRewardTable,from_address,startRewardBlockNumber)
        print("user not exsit,add user ")
    else

        user_origin_balance = safemath.bigint(tostring(user_originBalance))
        --exec user_pay

        let lastRewardBlock = fast_map_get(lastRewardTable,from_address)
        if not lastRewardBlock then
            return error("user obtain unkown error!")
        end

        if safemath.gt(user_origin_balance,safemath.bigint(0))  then

            let reward = cal_user_can_reward(self,stakingCoinAddr,from_address,lastRewardBlock)

            let stakingTokenContract: object = import_contract_from_address(self.storage.tokenAddress)
            if tointeger(reward) > tointeger(0) then
                fast_map_set(lastRewardTable,from_address,tostring(get_header_block_num()))

                self.storage.StakingRewardSupply[stakingCoinAddr] = safemath.toint(safemath.add(safemath.bigint(self.storage.StakingRewardSupply[stakingCoinAddr]),safemath.bigint(tointeger(reward))))

                if self.storage.StakingRewardSupply[stakingCoinAddr] > self.storage.StakingRewardLimit[stakingCoinAddr] then

                    return error("The money supply exceeds the maximum supply after the user receives the amount")
                end
                let rewardStr = from_address ..","..tostring(reward)
                stakingTokenContract:mint(rewardStr)
            end
            
            
            
        end
    end

    let user_end_balance = safemath.tostring(safemath.add(user_origin_balance,safemath.bigint(amount)))

    fast_map_set(balanceTable,from_address,user_end_balance)
    let total_balance = safemath.tostring(safemath.add(safemath.bigint(self.storage.allSymbolTotalBalance[stakingCoinAddr]),safemath.bigint(amount)))
    self.storage.allSymbolTotalBalance[stakingCoinAddr] = total_balance

end


-- arg: name,symbol,tokenAddr
function M:init_config(arg: string)
    checkAdmin(self)
    check_caller_frame_valid(self)
    pprint('arg:', arg)
    if self.storage.state ~= 'NOT_INITED' then
        return error("this token contract inited before")
    end
    let parsed = parse_args(arg, 3, "argument format error, need format: name,symbol,tokenAddr")
    let info = {name: parsed[1], symbol: parsed[2], tokenAddr: parsed[3]}
    if not info.name then
        return error("name needed")
    end
    self.storage.name = tostring(info.name)
    if not info.symbol then
        return error("symbol needed")
    end
    self.storage.symbol = tostring(info.symbol)
    if not info.tokenAddr then
        return error("tokenAddr needed")
    end
    let token_addr = tostring(info.tokenAddr)
    
    self.storage.tokenAddress = token_addr

    let from_address = get_from_address()
    if from_address ~= caller_address then
        return error("init_token can't be called from other contract")
    end

    self.storage.state = 'COMMON'
    emit Inited(arg)
    
    
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

offline function M:fee(_: string)
    let feeStr = tostring(self.storage.fee)
    return feeStr
end


function M:pause(arg: string)
    check_caller_frame_valid(self)
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
    check_caller_frame_valid(self)
    if self.storage.state ~= 'PAUSED' then
        return error("this contract not paused now, can't resume")
    end
    checkAdmin(self)
    self.storage.state = 'COMMON'
    emit Resumed("")
end

function M:stop(arg: string)
    check_caller_frame_valid(self)
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



-- stakingCoinAddr,amount,
function M:staking(arg:string)
    checkState(self)
    checkMigration(self)
    let parsed = parse_args(arg, 2, "argument format error, need format:  stakingCoinAddr,amount")
    let stakingCoinAddr = tostring(parsed[1])
    let amount = safemath.bigint(tostring(parsed[2]))
    let from_address = get_from_address()
    let is_native_asset = is_native_asset_symbol(stakingCoinAddr)
    if is_native_asset then
        return error("Please use the method of transferring money to the contract for staking the native assets!")
    end

    -- check stakingCoinAddr has set
    let token_symbol = self.storage.allTokenSymbol[stakingCoinAddr]
    if not token_symbol  then
        return error("The contract address is not currently supported for mining!")
    end
    print("-0--------")
    print(safemath.tostring(amount))
    if (not amount) or (safemath.lt(amount , safemath.bigint(0))) then

		 return error("deposit should greater than 0")
	end
    print("-0--------2")

    let stakingTokenContract: object = import_contract_from_address(stakingCoinAddr)
    let cur_contract = get_current_contract_address()
    let prifixstr = from_address ..","..cur_contract..","
    print("-0--------21")
    let origin_balance = stakingTokenContract:balanceOf(cur_contract)
    print(origin_balance)
    print("-0--------22")
    stakingTokenContract:transferFrom(prifixstr..safemath.tostring(amount))
    print("-0--------23")
    let new_balance = stakingTokenContract:balanceOf(cur_contract)
    print("-0--------3")
    if safemath.lt( safemath.sub(safemath.bigint(tostring(new_balance)) , safemath.bigint(tostring(origin_balance))) ,amount) then
        print(new_balance)
        print(origin_balance)
        print(safemath.tostring(amount))
        print("-11---")
        let eventStr = json.dumps({from_address:from_address,origin_amount:safemath.tostring(amount),new_balance:safemath.tostring(safemath.sub( safemath.bigint(new_balance) , safemath.bigint(origin_balance)))})
        emit INEXACTDEPOSIT(eventStr)
        amount = safemath.sub(safemath.bigint(new_balance) , safemath.bigint(origin_balance))
    end
    print("-0--------45")
    if (not amount) or (safemath.lt(amount , safemath.bigint(0))) then
        print(safemath.tostring(amount))
		 return error("deposit should greater than 0")
	end
    print("-0--------")
	-- check user exist
    -- SymboluserBalance   fastMap  addr->balance
    -- SymbolUserAddress   fastMap  id -> addr
    let balanceTable = token_symbol.."userBalance"
    let addressTable = token_symbol.."userAddress"
    let lastRewardTable = token_symbol.."userRewardBlockNum"
    let user_originBalance = fast_map_get(balanceTable,from_address)
    let user_origin_balance = safemath.bigint(0)

    if user_originBalance == nil  then
        -- user not exsit
        -- add user 
        let symbolUserCount = tointeger(self.storage.allTokenUserCount[stakingCoinAddr])
        
        self.storage.allTokenUserCount[stakingCoinAddr] =  symbolUserCount + tointeger(1)
        fast_map_set(addressTable,tostring(symbolUserCount),from_address)
        let current_block = get_header_block_num()
        let startRewardBlockNumber = math.max(tointeger(current_block),tointeger(self.storage.stakingRewardStartBlock[stakingCoinAddr]))
        fast_map_set(lastRewardTable,from_address,startRewardBlockNumber)
    else
        user_origin_balance = safemath.bigint(tostring(user_originBalance))
        --exec user_pay
       

            let lastRewardBlock = fast_map_get(lastRewardTable,from_address)
            if not lastRewardBlock then
                return error("user obtain unkown error!")
            end
            
        if safemath.gt( user_origin_balance, safemath.bigint(0)) then
            let reward = cal_user_can_reward(self,stakingCoinAddr,from_address,lastRewardBlock)
            
            let stakingTokenContract: object = import_contract_from_address(self.storage.tokenAddress)
            
            if tointeger(reward) > tointeger(0) then
                fast_map_set(lastRewardTable,from_address,tostring(get_header_block_num()))
                self.storage.StakingRewardSupply[stakingCoinAddr] = tointeger(self.storage.StakingRewardSupply[stakingCoinAddr])+tointeger(reward)
                if self.storage.StakingRewardSupply[stakingCoinAddr] > self.storage.StakingRewardLimit[stakingCoinAddr] then
                    return error("The money supply exceeds the maximum supply after the user receives the amount")
                end
                let rewardStr = from_address ..","..tostring(reward)
                stakingTokenContract:mint(rewardStr)
            end
            

        end
    end
print("-2--------")
    let user_end_balance = safemath.tostring(safemath.add(user_origin_balance,amount))
    fast_map_set(balanceTable,from_address,user_end_balance)
    let total_balance = safemath.tostring(safemath.add(safemath.bigint(self.storage.allSymbolTotalBalance[stakingCoinAddr]),amount))
    self.storage.allSymbolTotalBalance[stakingCoinAddr] = total_balance

end

--arg stakingCoinAddr,Amount,isAll
function M:withdraw(arg:string)
    checkState(self)
    let parsed = parse_at_least_args(arg, 2, "argument format error, need format:  stakingCoinAddr,amount,isAll")
    let stakingCoinAddr = tostring(parsed[1])
    let is_native_asset = is_native_asset_symbol(stakingCoinAddr)
    let amount = safemath.bigint(tostring(parsed[2]))
    let isAll = tointeger(0)
    if #parsed >2 then
        isAll = tointeger(parsed[3])
    end
    let from_address = get_from_address()

    if (not amount) or (safemath.lt (amount ,safemath.bigint(0))) then
		 return error("withdraw should greater than 0")
	end
    -- check stakingCoinAddr has set
    let token_symbol = self.storage.allTokenSymbol[stakingCoinAddr]
    if not token_symbol  then
        return error("The contract address is not currently supported for mining!")
    end

    let balanceTable = token_symbol.."userBalance"
    let addressTable = token_symbol.."userAddress"
    let lastRewardTable = token_symbol.."userRewardBlockNum"
    let user_originBalance = fast_map_get(balanceTable,from_address)
    if user_originBalance == nil then
        return error("The user did not recharge this asset.contract addr "..stakingCoinAddr)
    end
    if isAll >tointeger(0) then
        amount =  safemath.bigint(user_originBalance)
    end
    if safemath.eq(amount,safemath.bigint(0)) then
        return 
    end
    if safemath.gt(amount ,safemath.bigint(user_originBalance))  then
        return error("The user cannot withdraw due to insufficient balance")
    end
    let lastRewardBlock = fast_map_get(lastRewardTable,from_address)
    if not lastRewardBlock then
        return error("user obtain unkown error!")
    end
    

    let reward = cal_user_can_reward(self,stakingCoinAddr,from_address,lastRewardBlock)
    --withdraw asset
    if is_native_asset then
        -- 如果token1是原生资产
        withdraw_native_asset_private(self, from_address, stakingCoinAddr, amount)
        
    else
        -- 如果token1是合约代币
        let newBalance = safemath.sub( safemath.bigint( user_originBalance) , amount)
        fast_map_set(balanceTable, from_address, safemath.tostring( newBalance))
        self.storage.allSymbolTotalBalance[stakingCoinAddr] = safemath.tostring(safemath.sub( safemath.bigint( self.storage.allSymbolTotalBalance[stakingCoinAddr]) , amount))
        let token1_contract = import_contract_from_address(stakingCoinAddr)
        token1_contract:transfer(from_address..","..safemath.tostring(amount))
        let nativeTransferEventArg = json.dumps({
        address: from_address,
        symbol: token_symbol,
        change: - safemath.toint(amount),
        reason: 'withdraw'
        })
        emit NativeBalanceChange(nativeTransferEventArg)
    end
    if tointeger(reward) > tointeger(0) and self.storage.isMigration == false then
        let stakingTokenContract: object = import_contract_from_address(self.storage.tokenAddress)
        fast_map_set(lastRewardTable,from_address,tostring(get_header_block_num()))
        self.storage.StakingRewardSupply[stakingCoinAddr] = tointeger(self.storage.StakingRewardSupply[stakingCoinAddr])+tointeger(reward)
        if self.storage.StakingRewardSupply[stakingCoinAddr] > self.storage.StakingRewardLimit[stakingCoinAddr] then
            return error("The money supply exceeds the maximum supply after the user receives the amount")
        end
            
         
        let rewardStr = from_address ..","..tostring(reward)

        stakingTokenContract:mint(rewardStr)
        let eventStr = json.dumps({address:from_address,obtainReward:reward})
        emit ObtainReward(eventStr)
    end
    



end

--arg  coinAddr,coinSymbol,total_supply,blocknumber,reward,blocknumber,reward,blocknumber,0
function M:setStakingContract(arg:string)
    checkAdmin(self)
    let parsed = parse_at_least_args(arg, 5, "argument format error, need format:  coinAddr,coinSymbol,startRewardBlock,endRewardBlock,OneBlockReward")
    let info = {coinAddr: parsed[1], coinSymbol: parsed[2], totalSupply: tointeger(parsed[3])}
    -- check contract addr exsit
    -- check symbol exist
    -- set reward
    -- set startNumber
    if self.storage.allTokenSymbol[info.coinAddr] ~= nil then
        return error("token already exist! can't add again!")
    end
    for _,v in pairs(self.storage.allTokenSymbol) do 
        if v == info.coinSymbol then
            return error("sumbol already exist! ")
        end
    end
    let storeReward = []
    if((#parsed-3) %2 ~=0) then
        return error("argument format error, need format is contract_addr,Symbol,blockNumber,reward,blockNumber,reward ...")
    end
    var i :int
   -- print(#parsed-2)
   let max_blockNumber =tointeger(get_header_block_num())
    for i=1,(#parsed-3),1 do
        if (i %2 ~=0) then
            if (max_blockNumber > tointeger(parsed[i+3])) then
                return error("RewardStr is not sorted in block high order. ")
            else
                max_blockNumber = tointeger(parsed[i+3])
            end
        end
        storeReward[i] = tointeger(parsed[i+3])
        
    end


    self.storage.StakingRewardConfig[info.coinAddr] = json.dumps(storeReward)
    self.storage.StakingRewardSupply[info.coinAddr] = tointeger(0)
    self.storage.StakingRewardLimit[info.coinAddr] = info.totalSupply
    self.storage.allTokenSymbol[info.coinAddr] = info.coinSymbol
    self.storage.allTokenUserCount[info.coinAddr] = 0
    self.storage.allSymbolTotalBalance[info.coinAddr] = "0"
    self.storage.stakingRewardStartBlock[info.coinAddr] = tointeger(storeReward[1])
    info["RewardStr"]=self.storage.StakingRewardConfig[info.coinAddr]

    let eventStr = json.dumps(info)
    emit CreateNewStakingPool(eventStr)
end

-- coinAddr,coinSymbol,total_supply,blocknumber,reward,blocknumber,reward,blocknumber,0
function M:changeStakingReward(arg:string)
    checkAdmin(self)
    let parsed = parse_at_least_args(arg, 5, "argument format error, need format:  coinAddr,coinSymbol,startRewardBlock,endRewardBlock,OneBlockReward")
    let info = {coinAddr: parsed[1], coinSymbol: parsed[2], totalSupply: tointeger(parsed[3])}
    if self.storage.allTokenSymbol[info.coinAddr] ~= info.coinSymbol then
        return error("Please correctly select the Staking ore pool to be modified")
    end
    let storeReward = []
    if((#parsed-3) %2 ~=0) then
        return error("argument format error, need format is contract_addr,Symbol,blockNumber,reward,blockNumber,reward ...")
    end
    var i :int
   -- print(#parsed-2)
   let max_blockNumber =tointeger(get_header_block_num())
    for i=1,(#parsed-3),1 do
        if (i %2 ~=0) then
            if (max_blockNumber > tointeger(parsed[i+3])) then
                return error("RewardStr is not sorted in block high order. ")
            else
                max_blockNumber = tointeger(parsed[i+3])
            end
        end
        storeReward[i] = tointeger(parsed[i+3])
        
    end
    self.storage.StakingRewardConfig[info.coinAddr] = json.dumps(storeReward)
    self.storage.StakingRewardLimit[info.coinAddr] = info.totalSupply
    self.storage.stakingRewardStartBlock[info.coinAddr] = tointeger(storeReward[1])
    info["RewardStr"]=self.storage.StakingRewardConfig[info.coinAddr]
    let eventStr = json.dumps(info)
    emit ChangeStakingPoolReward(eventStr)
end

--coinAddr
function M:obtainReward(arg:string)
    checkState(self)
    let parsed = parse_args(arg,1,"argument format error, need format:  coinAddr")
    let stakingCoinAddr = parsed[1]
    let from_address = get_from_address()
    if self.storage.allTokenSymbol[stakingCoinAddr] ~= nil then
        let token_symbol = self.storage.allTokenSymbol[stakingCoinAddr]
    
        let balanceTable = token_symbol.."userBalance"
        let addressTable = token_symbol.."userAddress"
        let lastRewardTable = token_symbol.."userRewardBlockNum"
        --let user_origin_balance = safemath.bigint(fast_map_get(balanceTable,from_address) or 0)
        --exec user_pay


        let lastRewardBlock = fast_map_get(lastRewardTable,from_address)
        if not lastRewardBlock then
            return error("user obtain unkown error!")
        end
        

        let reward = cal_user_can_reward(self,stakingCoinAddr,from_address,lastRewardBlock)
        print("reward",reward,from_address)
        if tointeger(reward) > tointeger(0)  and self.storage.isMigration == false then
            let stakingTokenContract: object = import_contract_from_address(self.storage.tokenAddress)
            fast_map_set(lastRewardTable,from_address,tostring(get_header_block_num()))
            self.storage.StakingRewardSupply[stakingCoinAddr] = tointeger(self.storage.StakingRewardSupply[stakingCoinAddr])+tointeger(reward)
            if  tointeger(self.storage.StakingRewardSupply[stakingCoinAddr]) > tointeger(self.storage.StakingRewardLimit[stakingCoinAddr]) then
                
                return error("The money supply exceeds the maximum supply after the user receives the amount")
            end
            
         
            let rewardStr = from_address ..","..tostring(reward)
            stakingTokenContract:mint(rewardStr)
            let eventStr = json.dumps({address:from_address,obtainReward:reward})
            emit ObtainReward(eventStr)
        end
    end
end

-- 升级修改token合约minter为新地址
-- new_staking_addr,
function M:migration(new_staking_addr:string)
    checkAdmin(self)
    checkMigration(self)
    checkAddress(new_staking_addr)
    let stakingTokenContract: object = import_contract_from_address(self.storage.tokenAddress)
    stakingTokenContract:changeMinter(new_staking_addr)
    self.storage.isMigration = true


end

-- 查询收入
--coinAddr,user_address
offline function M:queryReward(arg:string)
    checkState(self)
    let parsed = parse_args(arg,2,"argument format error, need format:  coinAddr,userAddress")
    let stakingCoinAddr = parsed[1]
    let userAddress = parsed[2]
    checkAddress(userAddress)
    if self.storage.allTokenSymbol[stakingCoinAddr] ~= nil then
        let lastRewardTable = self.storage.allTokenSymbol[stakingCoinAddr].."userRewardBlockNum"

         let lastRewardBlock = fast_map_get(lastRewardTable,userAddress)
        if not lastRewardBlock then
            return 0
        end
        let reward = cal_user_can_reward(self,stakingCoinAddr,userAddress,lastRewardBlock)
        return reward
    end
    return 0
end

--查询基本信息
offline function M:getInfo()
    let res = []
    
    for k,v in pairs(self.storage.allTokenSymbol) do 
        let info = {}
        info["coinAddr"] = k
        info["coinSymbol"] = v
        info["coinUserCount"] = self.storage.allTokenUserCount[k]
        info["coinTotalAmount"] = self.storage.allSymbolTotalBalance[k]
        info["startRewardBlock"] = self.storage.stakingRewardStartBlock[k]
        info["rewardConfig"] = self.storage.StakingRewardConfig[k]
        info["rewardLimit"] = self.storage.StakingRewardLimit[k]
        info["rewardSupply"] = self.storage.StakingRewardSupply[k]
        print(info,k,v)
        table.insert(res,info)
    end
    return json.dumps(res)
end

-- 查询质押金额
offline function M:queryStakingAmount(arg:string)
    checkState(self)
    let parsed = parse_args(arg,2,"argument format error, need format:  coinAddr,useraddr")
    let stakingCoinAddr = parsed[1]
    let userAddr = parsed[2]
    let from_address = tostring(userAddr)
     -- check stakingCoinAddr has set
    let token_symbol = self.storage.allTokenSymbol[stakingCoinAddr]
    if not token_symbol  then
        return error("The contract address is not currently supported for mining!")
    end

	-- check user exist
    -- SymboluserBalance   fastMap  addr->balance
    -- SymbolUserAddress   fastMap  id -> addr
    let balanceTable = token_symbol.."userBalance"
    return tointeger(fast_map_get(balanceTable,from_address) or 0)
end

    -- 查询总staking状态
offline function M:queryStakingState(arg:string)
    let coinAddr = arg
    let info = {}
    info["coinAddr"] = coinAddr
    info["coinSymbol"] = self.storage.allTokenSymbol[coinAddr]
    info["coinUserCount"] = self.storage.allTokenUserCount[coinAddr]
    info["coinTotalAmount"] = self.storage.allSymbolTotalBalance[coinAddr]
    info["startRewardBlock"] = self.storage.stakingRewardStartBlock[coinAddr]
    info["rewardConfig"] = self.storage.StakingRewardConfig[coinAddr]
    info["rewardLimit"] = self.storage.StakingRewardLimit[coinAddr]
    info["rewardSupply"] = self.storage.StakingRewardSupply[coinAddr]
    return json.dumps(info)
end

offline function M:admin()
    return self.storage.admin
end
offline function M:totalAmount()
    return self.storage.allSymbolTotalBalance
end

offline function M:allTokenSymbol()
    return self.storage.allTokenSymbol
end

offline function M:allTokenUserCount()
    return self.storage.allTokenUserCount
end

offline function M:IsMigration()
    return self.storage.isMigration
end


function M:on_destroy()
    error("can't destroy token contract")
end

return M
