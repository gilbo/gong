--[[
  verbosity - for controlling debugging, and information dumps

  See LICENSE
--]]

local Exports = {}
package.loaded["gong.src.verbosity"] = Exports

-------------------------------------------------------------------------------
-- Core verbosity level
-------------------------------------------------------------------------------

if not rawget(_G,'VERBOSITY') then
	_G['VERBOSITY'] = 0
end

function Exports.set_verbosity(level)
	_G['VERBOSITY'] = level
	if level > 0 then 
		print("GONG VERBOSITY SET TO "..tostring(level))
	end
end

function Exports.get_verbosity()
	if not rawget(_G,'VERBOSITY') then
		return 0
	else
		return _G['VERBOSITY']
	end
end

-------------------------------------------------------------------------------
-- Observable verbosity flags
-------------------------------------------------------------------------------

function Exports.is_PTXverbose()
	return Exports.get_verbosity() > 2
end

function Exports.is_PTXverbose()
	return Exports.get_verbosity() > 2
end
