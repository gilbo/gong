import 'gong'
local test  = require 'tests.test'

local G     = gong.stdlib


-- basic example
local Verts     = G.NewTable('Verts')
local Edges     = G.NewTable('Edges')
local Tris      = G.NewTable('Tris')

Verts:NewField('pos', G.vec3f)
Edges:NewField('hd',  Verts)
     :NewField('tl',  Verts)
Tris:NewField('v',    G.vector(Verts, 3))

-- that should all have been worked through just fine

test.eq(Verts:name(), 'Verts')
test.eq(Edges:name(), 'Edges')
test.eq(Tris:name(),  'Tris')

test.eq(Verts:is_complete(),  false)
test.eq(Edges:is_complete(),  false)
test.eq(Tris:is_complete(),   false)

Verts:complete()

test.fail(function()
  Verts:NewField('bb', G.bool)
end,'cannot add fields to a completed table')

test.fail(function()
  Edges:NewField('bb')
end,'expected gong type as arg')

test.fail(function()
  Edges:NewField(G.bool, 'bb')
end,'expected valid name string as arg 1')

test.fail(function()
  Tris:NewField('v', G.vec3f)
end,"table 'Tris' already has a field named 'v'")

test.eq(Verts:join_policy(),  'none')
test.eq(Edges:join_policy(),  'none')
test.eq(Tris:join_policy(),   'none')

test.eq(#(Verts:fields()),    1)
test.eq(#(Edges:fields()),    2)
test.eq(#(Tris:fields()),     1)

test.eq(Verts:fields(1),      Verts.pos)
test.eq(Edges:fields(1),      Edges.hd)
test.eq(Edges:fields(2),      Edges.tl)
test.eq(Tris:fields(1),       Tris.v)

test.eq(Verts:fields('pos'),  Verts.pos)
test.eq(Edges:fields('hd'),   Edges.hd)
test.eq(Edges:fields('tl'),   Edges.tl)
test.eq(Tris:fields('v'),     Tris.v)

test.aeq(Verts:fields(), {Verts.pos})
test.aeq(Edges:fields(), {Edges.hd, Edges.tl})
test.aeq(Tris:fields(),  {Tris.v})

test.eq(Verts.pos:fullname(), 'Verts.pos')
test.eq(Verts.pos:name(), 'pos')
test.eq(Edges.hd:type(), G.row(Verts))
test.eq(Edges.hd:table(), Edges)

test.eq(Edges:record_type(),
        G.record { {'hd',G.row(Verts)}, {'tl',G.row(Verts)} })

-- table errors

test.fail(function()
  Tris.foo = 3
end, "Cannot assign members")

test.fail(function()
  Tris.v.foo = 3
end, "Cannot assign members")

test.fail(function()
  G.NewTable()
end, "expected valid name string as first argument")
test.fail(function()
  G.NewTable('@@@')
end, "expected valid name string as first argument")

test.fail(function()
  G.NewTable('foos', { join_policy = 'splatooie' })
end, "unrecognized join policy: splatooie")






