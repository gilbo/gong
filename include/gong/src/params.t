--[[
  params - control parameterization of the rest of the system

  See LICENSE
--]]

local Exports = {}
package.loaded["gong.src.params"] = Exports

-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------

-- set default gpu enabled option based on
-- auto-detecting whether functionality is available
local default_gpu_on = not not (terralib.cudacompile)

local default_params = {
  -- expect to be modified parameters
  ['GPU_ENABLED']       = default_gpu_on,

  -- potentially modifiable parameters
  ['GPU_BLOCK_SIZE']    = 64,
  ['GPU_RAND_BUF_SIZE'] = 1e6, -- 1 million * 4 bytes = 4MB storage

  -- Constants that should almost certainly not be changed
  ['WARPSIZE']          = 32,
  ['GPU_MAX_BLOCK_DIM'] = 65536,
  ['GPU_RAND_MAX']      = 1.0e32-1.0,
}
local param_table = {}
for k,v in pairs(default_params) do param_table[k] = v end


function Exports.get_param(key)
  return assert(param_table[key])
end

function Exports.set_param(key,val)
  param_table[key] = val
end
