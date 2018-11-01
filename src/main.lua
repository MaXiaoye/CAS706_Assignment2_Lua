--Extend Env by adding "Var=Value" pair into table
function extendEnv(x,v,env)
  env[x] = v
  return env
end

--Find value of a variable in env.
function lookup(x,env)
  return env[x]
end

--Eval rules
--look for value in env when deal with variable.
--return value or bool when deal with value.
--return closure containing lambda and env when deal with lambda.
--Apply1: return just "a b" when deal with a b if a and b are just value or can't be reducted, if a is a Lambda then do recursively reduction that apply a to b. 
--recusively deal with applicaion (Apply2) and store Val=Value in env.
--recusively deal with arithmetic and bool expression.
local function interp(term,env)
  if term == true then
    return true
  elseif term == false then
    return false 
  elseif term.type == 'Val' then
    return term.value
  elseif term.type == 'Var' then
    if env == nil or lookup(term.name,env) == nil then
      return term.name
    else
      return lookup(term.name,env)
    end
  elseif term.type == 'Lambda' then
    return Closure:new(term,env)
  elseif term.type == 'Let' then
    if term.exp1.type ~= 'Var' 
      then print("Invalid Input")
    else
      local v1 = interp(term.exp2, env)
      if type(v1) == 'table' and v1.type == 'Closure' then
        v1 = v1.exp 
      end
      return interp(term.exp3,extendEnv(term.exp1.name,v1,env))
    end
  elseif term.type == 'Apply1' then
    local a1 = interp(term.exp1,env)
    local a2 = interp(term.exp2,env)
    if type(a1) == 'table' and (a1.type == 'Lambda' or a1.type == 'Apply2') then     
      return interp(Apply2:new(a1,term.exp2),{})
    else
      return a1..' '..a2
    end
  elseif term.type == 'Apply2' then
    local v1 = interp(term.exp1, env)
    local v2 = interp(term.exp2, {})
    if type(v2) == 'table' and v2.type == 'Closure' then
      v2 = v2.exp 
    end
    if v1.type == 'Closure' then    
      return interp(v1.exp.body,extendEnv(v1.exp.arg.name,v2,v1.env))
    else
      print("Invalid Input")
    end 
  elseif term.type == 'Calc' then
    local e1 = interp(term.arg1, env)
    local e2 = interp(term.arg2, env)
    if type(e1) == 'number' and type(e2) == 'number' then
      if term.op == '+' then
        return e1+e2
      elseif term.op == '-' then
        return e1-e2
      elseif term.op == '*' then
        return e1*e2
      elseif term.op == '/' then
        return e1/e2
      else
        print("Invalid Input")
      end
    else 
      if term.op == '+' then 
        return e1..'+'..e2
      elseif term.op == '-' then 
        return e1..'-'..e2
      elseif term.op == '*' then 
        return e1..'*'..e2
      elseif term.op == '/' then 
        return e1..'/'..e2
      else
        print("Invalid Input")
      end
    end
  elseif term.type == 'Bool' then
    return term.value
  elseif term.type == 'not' then    
    return not interp(term.value,env)
  elseif term.type == 'and' then
    return (interp(term.arg1,env) and interp(term.arg2,env))
  elseif term.type == 'or' then
    return (interp(term.arg1,env) or interp(term.arg2,env))
  elseif term.type == 'Lt' then
    return (interp(term.arg1,env) < interp(term.arg2,env))
  elseif term.type == 'Lte' then
    return (interp(term.arg1,env) <= interp(term.arg2,env))
  elseif term.type == 'If' then
    local v1 = interp(term.exp1,env)
    if v1 then
      return interp(term.exp2,env)
    else
      return interp(term.exp3,env)
    end
  else
    print("Invalid Input")
  end
end

--Class for value.
Val = {type = 'Val', value = 0}
Val.__index = Val
function Val:new (value1)
  local self = {} 
  setmetatable(self, Val)
  self.type = 'Val'
  self.value = value1
  return self
end 

--Class for variable
Var = {type = 'Var', name = ''}
Var.__index = Var
function Var:new (name)
  local self = {}
  setmetatable(self, Var)
  self.type = 'Var'
  self.name = name
  return self
end

--Class for op2 arithmetic expression +,-,*,/.
Calc = {type = 'Calc', op = '', arg1 = {}, arg2 = {}}
Calc.__index = Calc
function Calc:new (op, arg1, arg2)
  local self = {}
  setmetatable(self, Calc)
  self.type = 'Calc'
  self.op = op
  self.arg1 = arg1
  self.arg2 = arg2
  return self
end

--Class for Bool.
Bool = {type = 'Bool', value = {}}
Bool.__index = Bool
function Bool:new (value1)
  local self = {} 
  setmetatable(self, Bool)
  self.type = 'Bool'
  self.value = value1
  return self
end

--Class for not.
Not = {type = 'not', value = {}}
Not.__index = Not
function Not:new (value1)
  local self = {} 
  setmetatable(self, Not)
  self.type = 'not'
  self.value = value1
  return self
end

--Class for or.
Or = {type = 'or', arg1 = {}, arg2 = {}}
Or.__index = Or
function Or:new (arg1, arg2)
  local self = {} 
  setmetatable(self, Or)
  self.type = 'or'
  self.arg1 = arg1
  self.arg2 = arg2
  return self
end

--Class for and.
And = {type = 'and', arg1 = {}, arg2 = {}}
And.__index = And
function And:new (arg1, arg2)
  local self = {} 
  setmetatable(self, And)
  self.type = 'and'
  self.arg1 = arg1
  self.arg2 = arg2
  return self
end

--Class for equal.
Equ = {type = 'equ', arg1 = {}, arg2 = {}}
Equ.__index = Equ
function Equ:new (arg1, arg2)
  local self = {} 
  setmetatable(self, Equ)
  self.type = 'equ'
  self.arg1 = arg1
  self.arg2 = arg2
  return self
end

--Class for less than.
Lt = {type = 'Lt', arg1 = {}, arg2 = {}}
Lt.__index = Lt
function Lt:new (arg1, arg2)
  local self = {} 
  setmetatable(self, Lt)
  self.type = 'Lt'
  self.arg1 = arg1
  self.arg2 = arg2
  return self
end

--Class for less than or equal.
Lte = {type = 'Lte', arg1 = {}, arg2 = {}}
Lte.__index = Lte
function Lte:new (arg1, arg2)
  local self = {} 
  setmetatable(self, Lte)
  self.type = 'Lte'
  self.arg1 = arg1
  self.arg2 = arg2
  return self
end

--Class for if then else.
If = {type = 'If', exp1 = {}, exp2 = {}, exp3 = {}}
If.__index = If
function If:new (exp1, exp2, exp3)
  local self = {} 
  setmetatable(self, If)
  self.type = 'If'
  self.exp1 = exp1
  self.exp2 = exp2
  self.exp3 = exp3
  return self
end

--Classs for lambda item.
Lambda = {type = 'Lambda', arg = {}, body = {}}
Lambda.__index = Lambda
function Lambda:new (arg, body)
  local self = {}
  setmetatable(self, Lambda)
  self.type = 'Lambda'
  self.arg = arg
  self.body = body
  return self
end

--Class for closure
Closure = {type = 'Closure', exp = {}, env = {}}
Closure.__index = Closure
function Closure:new (exp, env)
  local self = {}
  setmetatable(self, Closure)
  self.type = 'Closure'
  self.exp = exp
  self.env = env
  return self
end

--Class for apply such as (\x \y a b)
Apply1 = {type = 'Apply1', exp1 = {}, exp2 = {}}
Apply1.__index = Apply1
function Apply1:new (exp1, exp2)
  local self = {}
  setmetatable(self, Apply1)
  self.__index = self
  self.type = 'Apply1'
  self.exp1 = exp1
  self.exp2 = exp2
  return self
end

--Class for let
Let = {type = 'Let', exp1 = {}, exp2 = {}, exp3 = {}}
Let.__index = Let
function Let:new (exp1, exp2, exp3)
  local self = {}
  setmetatable(self, Let)
  self.__index = self
  self.type = 'Let'
  self.exp1 = exp1
  self.exp2 = exp2
  self.exp3 = exp3
  return self
end

--Class for application
Apply2 = {type = 'Apply2', exp1 = {}, exp2 = {}}
Apply2.__index = Apply2
function Apply2:new (exp1, exp2)
  local self = {}
  setmetatable(self, Apply2)
  self.__index = self
  self.type = 'Apply2'
  self.exp1 = exp1
  self.exp2 = exp2
  return self
end

--Rename variables.
function rename(exp)
  if exp.type == 'Var' then return Var:new(exp.name..'0')
    elseif exp.type == 'Lambda' then return Lambda:new(Var:new(exp.arg.name..'0'),rename(exp.body))
    elseif exp.type == 'Let' then return Let:new(rename(exp.exp1),rename(exp.exp2),rename(exp.exp3))
    elseif exp.type == 'Apply1' then return Apply1:new(rename(exp.exp1),rename(exp.exp2))
    elseif exp.type == 'Apply2' then return Apply2:new(rename(exp.exp1),rename(exp.exp2))
    elseif exp.type == 'Calc' then return Calc:new(exp.op,rename(exp.arg1),rename(exp.arg2))
    elseif exp.type == 'Val' then return exp
    elseif exp.type == 'Bool' then return exp
    elseif exp.type == 'not' then return Not:new(rename(exp.value))
    elseif exp.type == 'and' then return And:new(rename(exp.arg1),rename(exp.arg2))
    elseif exp.type == 'or' then return Or:new(rename(exp.arg1),rename(exp.arg2))
    elseif exp.type == 'Lt' then return Lt:new(rename(exp.arg1),rename(exp.arg2))
    elseif exp.type == 'Lte' then return Lte:new(rename(exp.arg1),rename(exp.arg2))
    elseif exp.type == 'If' then return If:new(rename(exp.exp1),rename(exp.exp2),rename(exp.exp3))
  else
    print("Invalid Input!")
  end  
end

--User interface, create an empty env as initial env and do renaming
function interpE(exp)
  local result = interp(rename(exp),{})
  
  -- If result is a closure or Lambda then print Lambda expression, recusively sub body part firstly.
  if type(result) == 'table' and result.type == 'Closure' then
    print('Lambda('..result.exp.arg.name..').'..interp(result.exp.body,result.env))
  elseif type(result) == 'table' and result.type == 'Lambda' then
    print('Lambda('..result.arg.name..').'..interp(result.body,result.env))
  elseif result == nil then
    print('No result!')
  else
    print(result)
  end
end

-- test cases

-- val 5
local val3 = Val:new(5)
-- arithmetic expression (x+7)
local calc2 = Calc:new('+',Var:new('x'),Val:new(7))
-- lambda: (\x.(x+7))
local lambda2 = Lambda:new(Var:new('x'),calc2)
-- test case (\x.(x+7)) 5
local applyb = Apply2:new(lambda2,val3)
print('\nResult of test case (\\x.(x+7)) 5')
interpE(applyb)

-- val 2
local val2 = Val:new('a')
-- arithmetic expression (x-y)
local calc3 = Calc:new('-',Var:new('x'),Var:new('y'))
-- lambda: \x.\y x-y
local lambda3 = Lambda:new(Var:new('x'),Lambda:new(Var:new('y'),calc3))
-- test case ((\x.\y. (x-y))a) ((\x.(x+7))5),
local applyc = Apply2:new(Apply2:new(lambda3,val2),applyb)
print('\nResult of test case ((\\x.\\y. (x-y))a) ((\\x.(x+7))5)')
interpE(applyc)

-- val 'y'
local val1 = Val:new('y')
-- (\x. \y. y x)
local lambda5 = Lambda:new(Var:new('x'),Lambda:new(Var:new('y'), Apply1:new(Var:new('y'),Var:new('x'))))
-- test case for (\x.\y. y x) y
local applyf = Apply2:new(lambda5,val1)
print('\nResult of test case (\\x.\\y. y x) y')
interpE(applyf)

-- (\x. x y)
local lambda6 = Lambda:new(Var:new('x'),Apply1:new(Var:new('x'),val1))
-- (\x. y x)
local lambda7 = Lambda:new(Var:new('x'),Apply1:new(val1,Var:new('x')))
-- test case for (\x. x y) (\x. y x)
local applyg = Apply2:new(lambda6,lambda7)
print('\nResult of test case (\\x. x y) (\\x. y x)')
interpE(applyg)

-- (\x.\y. x y)
local lambda8 = Lambda:new(Var:new('x'),Lambda:new(Var:new('y'),Apply1:new(Var:new('x'),Var:new('y'))))
-- (\x.\y. x y) (\x. y x)
local applyh = Apply2:new(lambda8,lambda7)
print('\nResult of test case (\\x.\\y. x y) (\\x. y x)')
interpE(applyh)

-- (\x.(true and (Not x)))false
local lambda11 = Lambda:new(Var:new('x'),And:new(Bool:new(true),Not:new(Var:new('x'))))
local applyi = Apply2:new(lambda11,Bool:new(false))
print('\nResult of test case (\\x.(true and (Not x)))false')
interpE(applyi)

-- Let x=5 in (x+1)
local let1 = Let:new(Var:new('x'),Val:new(5),Calc:new('+',Var:new('x'),Val:new(1)))
print('\nResult of test case (Let x=5 in (x+1))')
interpE(let1)

-- (Let x=5 in (x+1)) < 2
print('\nResult of test case ((Let x=5 in (x+1)) < 2)')
interpE(Lt:new(let1,Val:new(2)))

-- If ((\x.(true and (Not x)))false) then (Let x=5 in (x+1)) else false
local If1 = If:new(applyi,let1,Bool:new(false))
print('\nResult of test case (If ((\x.(true and (Not x)))false) then (Let x=5 in (x+1)) else false)')
interpE(If1)

-- Invalid input: Let 1=5 in (x+1)
local let2 = Let:new(Val:new(1),Val:new(5),Calc:new('+',Var:new('x'),Val:new(1)))
print('\nResult of test case (Let 1=5 in (x+1))')
interpE(let2)