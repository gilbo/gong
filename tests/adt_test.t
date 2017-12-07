import 'gong.src.adt'
--
--  See LICENSE
--

local ADT A
  Var     = { name : string }
  Assign  = { lhs : Var, rhs : Expr }
  Expr    = Add { lhs : Expr, rhs : Expr }
          | Sub { lhs : Expr, rhs : Expr? }
          | Mul { lhs : Expr, rhs : Expr }
          | Div { lhs : Expr, rhs : Expr }
          | Num { val : number }
          | Ref { variable : Var }
  Input   = { stmts : Assign* }
end

function A.Assign:prettyprint()
  return self.lhs:prettyprint()..' = '..self.rhs:prettyprint()
end
function A.Add:prettyprint()
  return '('..self.lhs:prettyprint()..'+'..self.rhs:prettyprint()..')'
end
function A.Sub:prettyprint()
  if self.rhs then
        return '('..self.lhs:prettyprint()..'-'..self.rhs:prettyprint()..')'
  else  return '-'..self.lhs:prettyprint() end
end
function A.Mul:prettyprint()
  return '('..self.lhs:prettyprint()..'*'..self.rhs:prettyprint()..')'
end
function A.Div:prettyprint()
  return '('..self.lhs:prettyprint()..'/'..self.rhs:prettyprint()..')'
end
function A.Num:prettyprint()
  return tostring(self.val)
end
function A.Var:prettyprint()
  return self.name
end
function A.Ref:prettyprint()
  return self.variable:prettyprint()
end
function A.Input:prettyprint()
  local lines = terralib.newlist()
  for i,stmt in ipairs(self.stmts) do
    lines:insert(stmt:prettyprint())
  end
  print(lines:concat('\n'))
end


function A.Assign:exec(store)
  store[self.lhs.name] = self.rhs:exec(store)
end
function A.Add:exec(store)
  return self.lhs:exec(store) + self.rhs:exec(store)
end
function A.Sub:exec(store)
  if self.rhs then return self.lhs:exec(store) - self.rhs:exec(store)
              else return - self.lhs:exec(store) end
end
function A.Mul:exec(store)
  return self.lhs:exec(store) * self.rhs:exec(store)
end
function A.Div:exec(store)
  return self.lhs:exec(store) / self.rhs:exec(store)
end
function A.Num:exec(store)
  return self.val
end
function A.Ref:exec(store)
  local val = store[self.variable.name]
  if not val then error('variable '..self.variable.name..' undefined') end
  return val
end
function A.Input:exec(store)
  store = store or {}
  for i,stmt in ipairs(self.stmts) do
    stmt:exec(store)
    print('eval '..stmt.lhs.name..' = '..store[stmt.lhs.name])
  end
  return store
end



local prog = A.Input(terralib.newlist{
  A.Assign(A.Var('x'),
           A.Add(A.Num(2), A.Mul(A.Num(3),A.Num(5)))),
  -- x = 2 + 3 * 5  -- i.e. 17
  A.Assign(A.Var('y'),
           A.Add(A.Ref(A.Var('x')), A.Sub(A.Num(4)))),
  -- y = x - 4
  A.Assign(A.Var('z'),
           A.Div(A.Add(A.Ref(A.Var('x')),A.Ref(A.Var('y'))),A.Num(3))),
  -- z = (x+y)/3
  A.Assign(A.Var('x'),
           A.Sub(A.Ref(A.Var('y')),A.Ref(A.Var('z')))),
  -- x = y + z
})
print(prog)
prog:prettyprint()
prog:exec()



local function is_int(obj)
  return type(obj) == 'number' and obj%1 == 0
end

local ADT B
  NumList = NumCons { n : num, ns : NumList }
          | NumEmpty
  IntList = IntCons { n : int, ns : IntList }
          | IntEmpty
  Expr    = Opt1 { a : Expr, b : number }
          | Opt2 { c : number }
          attributes { foo : string }

  extern num function(obj) return type(obj) == 'number' end
  extern int is_int
end


local nslist = B.NumCons( 3, B.NumCons( 4.2, B.NumEmpty) )
assert(not pcall(function()
  local nslist = B.NumCons( 3, B.NumCons( 4.2, 6) )
end))

local ilist = B.IntCons( 3, B.IntCons( 4, B.IntEmpty) )
assert(not pcall(function()
  local ilist = B.IntCons( 3, B.IntCons( 4.2, B.IntEmpty) )
end))

local e = B.Opt2(32, 'ok')
assert(not pcall(function()
  local e = B.Opt1( B.Opt2(3), 9 )
end))




