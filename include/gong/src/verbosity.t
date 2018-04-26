local v = {}
if not rawget(_G,'VERBOSITY') then
	_G['VERBOSITY'] = 0
end

function v.set_verbosity(level)
	_G['VERBOSITY'] = level
	if level > 0 then 
		print("GONG VERBOSITY SET TO "..tostring(level))
	end
end

function v.get_verbosity()
	if not rawget(_G,'VERBOSITY') then
		return 0
	else
		return _G['VERBOSITY']
	end
end

return v