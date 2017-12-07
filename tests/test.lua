--NOT-A-TEST

local test = {}

function test.eq(a,b)
  if a ~= b then
    error(tostring(a) .. " ~= "  .. tostring(b),2)
  end
end
function test.neq(a,b)
  if a == b then
    error(tostring(a) .. " == "  .. tostring(b),2)
  end
end

function test.seteq(a,b)
  for id,_ in pairs(a) do
    if not b[id] then
      error('Element ' .. tostring(id) .. ' of the left-set was not found '..
            'in the right-set', 2)
    end
  end
  for id,_ in pairs(b) do
    if not a[id] then
      error('Element ' .. tostring(id) .. ' of the right-set was not found '..
            'in the left-set', 2)
    end
  end
end

function test.aeq(a, b)
  if #a ~= #b then error("Arrays are not of equal length", 2) end
  for i = 1, #a do
    if a[i] ~= b[i] then error("Element " .. tostring(i) .. " of arrays do not match (" .. tostring(a[i]) .. ", " .. tostring(b[i]) .. ")", 2) end
  end
end

function test.rec_aeq(a, b, idxstr)
  idxstr = idxstr or ''
  if type(a) ~= 'table' or type(b) ~= 'table' then
    if a ~= b then error("Element (index"..idxstr.." ) "..
                         "of arrays does not match "..
                         "( "..tostring(a).." vs. "..tostring(b).." )",
                         2) end
  else
    if #a ~= #b then error("(Sub-)arrays (index "..idxstr.." ) "..
                           "do not have matching length "..
                           "( "..tostring(#a).." vs. "..tostring(#b).." )",
                           2) end
    -- recurse
    for i=1,#a do
      test.rec_aeq(a[i],b[i], idxstr..' '..tostring(i))
    end
  end
end

-- for diagnosing
function test.rec_aprint(a, reccall)
  if type(a) ~= 'table' then
    if reccall then return tostring(a)
               else print(a) end
  else
    local str = '{'
    for i=1,#a do
      if i > 1 then str = str .. ',' end
      str = str .. test.rec_aprint(a[i], true)
    end
    str = str .. '}'
    if reccall then return str
               else print(str) end
  end
end

local zero_diff = .0000005
function test.fuzzy_eq (a, b)
  local d = a - b
  if d < 0 then d = -d end
  if d > zero_diff then error(tostring(a) .. " ~= " .. tostring(b),2) end
end
function test.fuzzy_aeq (a, b)
  if #a ~= #b then error("Arrays are not of equal length", 2) end
  for i = 1, #a do
    local d = a[i] - b[i]
    if d < 0 then d = -d end
    if d > zero_diff then error("Element " .. tostring(i) .. " of arrays do not match (" .. tostring(a[i]) .. ", " .. tostring(b[i]) .. ")", 2) end
  end
end
function test.fuzzy_rec_aeq(a, b, idxstr)
  idxstr = idxstr or ''
  if type(a) ~= 'table' or type(b) ~= 'table' then
    if type(a) ~= 'number' then
      idxstr = #idxstr > 0 and ("(index "..idxstr..") ") or ""
      error("Found a non-table, but also non-numeric value on the left. "..
            idxstr, 2)
    end
    if type(b) ~= 'number' then
      idxstr = #idxstr > 0 and ("(index "..idxstr..") ") or ""
      error("Found a non-table, but also non-numeric value on the right. "..
            idxstr, 2)
    end
    local d = math.abs(a - b)
    if d > zero_diff then
      idxstr = #idxstr > 0 and ("(index "..idxstr..") ") or ""
      error("Element "..idxstr.."of arrays does not match "..
            "( "..tostring(a).." vs. "..tostring(b).." )", 2)
    end
  else
    if #a ~= #b then
      idxstr = #idxstr > 0 and ("(index "..idxstr..") ") or ""
      error("(Sub-)arrays "..idxstr.."do not have matching length "..
            "( "..tostring(#a).." vs. "..tostring(#b).." )", 2)
    end
    -- recurse
    for i=1,#a do
      test.fuzzy_rec_aeq(a[i],b[i], idxstr..' '..tostring(i))
    end
  end
end

-- This code is based off tests/coverage.t from the terra project
function test.fail(fn, match)
  local msg = ''
  local function handler ( errobj )
    msg = tostring(errobj) .. '\n' .. debug.traceback()
  end
  local success = xpcall(fn, handler)
  if success then
    error("Expected function to fail, but it succeeded.", 2)
  elseif not string.match(msg,match) then
    error("Function did not produce the expected error: " .. msg, 2)
  end
end





return test