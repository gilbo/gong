
local Exports = {}
package.loaded["gong.src.profiling"] = Exports

local Util            = require 'gong.src.util'
local INTERNAL_ERR    = Util.INTERNAL_ERR

local PARAM           = (require 'gong.src.params').get_param
local PROFILING_OFF   = PARAM('PROFILING_OFF')

local C               = require 'gong.src.c'
local assert          = C.assert

local newlist         = terralib.newlist

-------------------------------------------------------------------------------
-- Profiling System
-------------------------------------------------------------------------------
-- Object Declarations

local ProfileMetric           = {}
ProfileMetric.__index         = ProfileMetric
local ProfileCounter          = setmetatable({}, ProfileMetric)
ProfileCounter.__index        = ProfileCounter
local ProfileReport           = setmetatable({}, ProfileMetric)
ProfileReport.__index         = ProfileReport
local ProfileElapsed          = setmetatable({}, ProfileMetric)
ProfileElapsed.__index        = ProfileElapsed
local ProfileFramedCounter    = setmetatable({}, ProfileMetric)
ProfileFramedCounter.__index  = ProfileFramedCounter

local function is_profile_metric(obj)
  return getmetatable(getmetatable(obj)) == ProfileMetric end
local function is_profile_report(obj)
  return getmetatable(obj) == ProfileReport end
local function is_profile_counter(obj)
  return getmetatable(obj) == ProfileCounter end
local function is_profile_elapsed(obj)
  return getmetatable(obj) == ProfileElapsed end
local function is_profile_framed_counter(obj)
  return getmetatable(obj) == ProfileFramedCounter end

local Profiler    = {}
Profiler.__index  = Profiler

local function is_profiler(obj) return getmetatable(obj) == Profiler end



------------------------------------------
-- Metric Counter Implementations

local struct CounterImplementation {
  _count    : uint32
  _padding  : uint32  -- align with doubles below
}
local struct ReportImplementation {
  _n_reports    : uint32
  _sum          : double
  _min          : double
  _max          : double
}
local struct ElapsedImplementation {
  -- runtime-state
  _start_time   : double
  -- stats
  _count        : uint32
  _sum_time     : double
  _min_time     : double
  _max_time     : double
}
local struct FramedCounterImplementation {
  -- runtime-state
  _frame_count  : uint32
  -- stats
  _n_frames     : uint32
  _sum_count    : double
  _min_count    : double
  _max_count    : double
}
ProfileCounter._implementation          = CounterImplementation
ProfileReport._implementation           = ReportImplementation
ProfileElapsed._implementation          = ElapsedImplementation
ProfileFramedCounter._implementation    = FramedCounterImplementation

local struct CounterOutput {
  count         : uint32
}
local struct ReportOutput {
  n_reports     : uint32
  avg           : double
  sum           : double
  min           : double
  max           : double
}
local struct ElapsedOutput {
  count         : uint32
  avg           : double
  sum           : double
  min           : double
  max           : double
}
local struct FramedCounterOutput {
  n_frames      : uint32
  avg           : double
  sum           : double
  min           : double
  max           : double
}

terra CounterImplementation:init()
  self._count         = 0
end
terra ReportImplementation:init()
  self._n_reports     = 0
  self._sum           = 0
  self._min           = [math.huge]
  self._max           = [-math.huge]
end
terra ElapsedImplementation:init()
  self._start_time    = 0
  self._count         = 0
  self._sum_time      = 0
  self._min_time      = [math.huge]
  self._max_time      = 0
end
terra FramedCounterImplementation:init()
  self._frame_count   = 0
  self._n_frames      = 0
  self._sum_count     = 0
  self._min_count     = [math.huge]
  self._max_count     = 0
end

terra CounterImplementation:count() return self._count                  end
--
terra ReportImplementation:avg()    return self._sum / self._n_reports  end
terra ReportImplementation:sum()    return self._sum                    end
terra ReportImplementation:min()    return self._min                    end
terra ReportImplementation:max()    return self._max                    end
terra ReportImplementation:n_reports()        return self._n_reports    end
--
terra ElapsedImplementation:avg()   return self._sum_time / self._count end
terra ElapsedImplementation:sum()   return self._sum_time               end
terra ElapsedImplementation:min()   return self._min_time               end
terra ElapsedImplementation:max()   return self._max_time               end
terra ElapsedImplementation:count() return self._count                  end
--
terra FramedCounterImplementation:avg()
                                return self._sum_count / self._n_frames end
terra FramedCounterImplementation:sum()       return self._sum_count    end
terra FramedCounterImplementation:min()       return self._min_count    end
terra FramedCounterImplementation:max()       return self._max_count    end
terra FramedCounterImplementation:n_frames()  return self._n_frames     end

terra CounterImplementation:output()
  return CounterOutput {
    count   = self:count()
  }
end
terra ReportImplementation:output()
  return ReportOutput {
    n_reports = self:n_reports(),
    avg       = self:avg(),
    sum       = self:sum(),
    min       = self:min(),
    max       = self:max(),
  }
end
terra ElapsedImplementation:output()
  return ElapsedOutput {
    count   = self:count(),
    avg     = self:avg(),
    sum     = self:sum(),
    min     = self:min(),
    max     = self:max(),
  }
end
terra FramedCounterImplementation:output()
  return FramedCounterOutput {
    n_frames  = self:n_frames(),
    avg       = self:avg(),
    sum       = self:sum(),
    min       = self:min(),
    max       = self:max(),
  }
end

terra CounterImplementation:increment()
  self._count         = self._count + 1
end
terra ReportImplementation:report( val : double )
  self._sum           = self._sum + val
  self._n_reports     = self._n_reports + 1
  if val > self._max then self._max = val end
  if val < self._min then self._min = val end
end
terra ElapsedImplementation:start()
  assert(self._start_time == 0, 'cannot start a running timer')
  self._start_time    = C.get_perf_time_in_seconds()
end
terra ElapsedImplementation:stop()
  var stop_time       = C.get_perf_time_in_seconds()
  assert(self._start_time ~= 0, 'cannot stop an already stopped timer')
  var diff_time       = stop_time - self._start_time
  self._start_time    = 0
  self._count         = self._count + 1
  self._sum_time      = self._sum_time + diff_time
  if diff_time < self._min_time then self._min_time = diff_time end
  if diff_time > self._max_time then self._max_time = diff_time end
end
terra FramedCounterImplementation:increment()
  self._frame_count   = self._frame_count + 1
end
terra FramedCounterImplementation:end_frame()
  var fc              = self._frame_count
  self._frame_count   = 0
  self._n_frames      = self._n_frames + 1
  self._sum_count     = self._sum_count + fc
  if fc > self._max_count then self._max_count = fc end
  if fc < self._min_count then self._min_count = fc end
end



------------------------------------------
-- Metric Counter Abstractions

local function NewProfileCounter(name, offset)
  return setmetatable({
    _name           = name,
    _offset         = offset,
    _impl           = CounterImplementation,
    _output         = CounterOutput,
  }, ProfileCounter)
end
local function NewProfileReport(name, offset)
  return setmetatable({
    _name           = name,
    _offset         = offset,
    _impl           = ReportImplementation,
    _output         = ReportOutput,
  }, ProfileReport)
end
local function NewProfileElapsed(name, offset)
  return setmetatable({
    _name           = name,
    _offset         = offset,
    _impl           = ElapsedImplementation,
    _output         = ElapsedOutput,
  }, ProfileElapsed)
end
local function NewProfileFramedCounter(name, offset)
  return setmetatable({
    _name           = name,
    _offset         = offset,
    _impl           = FramedCounterImplementation,
    _output         = FramedCounterOutput,
  }, ProfileFramedCounter)
end

function ProfileMetric:name()             return self._name       end
function ProfileMetric:offset()           return self._offset     end
function ProfileMetric:impl()             return self._impl       end
function ProfileMetric:output_struct()    return self._output     end
function ProfileMetric:sizeof()     return terralib.sizeof(self._impl)  end

function ProfileMetric:field_pair() return { self:name(), self:impl() } end

function ProfileMetric:impl_ptr( pptr )
  assert(pptr:gettype():ispointer(), 'INTERNAL: expect pointer')
  return quote
    var p   = [&uint8](pptr)
    p       = p + [self:offset()]
  in [&self:impl()](p) end
end
function ProfileCounter:increment( pptr )
  return quote
    var m   = [self:impl_ptr( pptr )]
    m:increment()
  end
end
function ProfileReport:report( pptr, val )
  return quote
    var m   = [self:impl_ptr( pptr )]
    m:report(val)
  end
end
function ProfileElapsed:start( pptr )
  return quote
    var m   = [self:impl_ptr( pptr )]
    m:start()
  end
end
function ProfileElapsed:stop( pptr )
  return quote
    var m   = [self:impl_ptr( pptr )]
    m:stop()
  end
end
function ProfileFramedCounter:increment( pptr )
  return quote
    var m   = [self:impl_ptr( pptr )]
    m:increment()
  end
end
function ProfileFramedCounter:end_frame( pptr )
  return quote
    var m   = [self:impl_ptr( pptr )]
    m:end_frame()
  end
end

function ProfileMetric:output( pptr )
  return quote
    var m   = [self:impl_ptr( pptr )]
  in m:output() end
end



------------------------------------------
-- Profiler Object

local function NewProfiler()
  --args = args or {}

  local struct ProfilerHandle { _id : &opaque }

  terra ProfilerHandle.methods.init     :: &ProfilerHandle -> {}
  terra ProfilerHandle.methods.destroy  :: &ProfilerHandle -> {}

  local profiler = setmetatable({
    _metrics        = newlist(),
    _metric_lookup  = {},
    _sum_offset     = 0,
    _type_handle    = ProfilerHandle,
    _struct         = nil,        -- to be generated later...
    _out_struct     = nil,
  }, Profiler)

  return profiler
end

function Profiler:handle()      return self._type_handle    end
function Profiler:is_complete() return self._struct ~= nil  end

function Profiler:complete()
  if self:is_complete() then return end
  local metrics         = self._metrics
  local MetricsStruct   = terralib.types.newstruct('ProfilerMetrics')
  local ProfilerOutput  = terralib.types.newstruct('ProfilerOutput')
  for _,m in ipairs(metrics) do
    MetricsStruct.entries:insert( m:field_pair() )
    ProfilerOutput.entries:insert( { m:name(), m:output_struct() } )
  end
  self._struct          = MetricsStruct
  self._out_struct      = ProfilerOutput
  local MS_size         = terralib.sizeof(MetricsStruct)

  local ProfilerHandle  = self._type_handle
  terra ProfilerHandle:init()
    var ms              = [&MetricsStruct](C.malloc(MS_size))
    self._id            = ms
    escape for _,m in ipairs(metrics) do emit quote
      ms.[ m:name() ]:init()
    end end end
  end
  terra ProfilerHandle:destroy()
    C.free(self._id)
    self._id = nil
  end

  terra ProfilerHandle:get_output() : ProfilerOutput
    var ms    = [&MetricsStruct](self._id)
    var out   : ProfilerOutput
    escape for _,m in ipairs(metrics) do emit quote
      out.[ m:name() ]  = ms.[ m:name() ]:output()
    end end end
    return out
  end
end

local valid_metric_types = {
  ['timer_start']         = true,
  ['timer_stop']          = true,
  ['counter']             = true,
  ['report']              = true,
  ['framed_counter']      = true,
  ['end_framed_counter']  = true,
}
local function add_metric_to_profiler( profiler, name, metric_str )
  assert(not profiler:is_complete(),
         'INTERNAL: cannot add metrics to a completed profiler')
  local off             = profiler._sum_offset

  local metric = nil
  if metric_str == 'counter' then
    metric              = NewProfileCounter(name, off)
  elseif metric_str == 'report' then
    metric              = NewProfileReport(name, off)
  elseif metric_str == 'timer_start' or metric_str == 'timer_stop' then
    metric              = NewProfileElapsed(name, off)
  elseif metric_str == 'framed_counter' or
         metric_str == 'end_framed_counter'
  then
    metric              = NewProfileFramedCounter(name, off)
  else
    INTERNAL_ERR('unrecognized profiler metric type: '..tostring(metric_str))
  end

  profiler._sum_offset            = off + metric:sizeof()
  profiler._metrics:insert(       metric  )
  profiler._metric_lookup[name]   = metric
  return metric
end
local function get_metric( profiler, name, metric_str )
  local m               = profiler._metric_lookup[name]
  if not m then
    m                   = add_metric_to_profiler(profiler, name, metric_str)
  end
  -- check metric type
  if    (metric_str == 'counter' and not is_profile_counter(m))
     or (metric_str == 'report' and not is_profile_report(m))
     or ( (metric_str == 'timer_start' or metric_str == 'timer_stop')
          and not is_profile_elapsed(m) )
     or ( (metric_str == 'framed_counter' or
           metric_str == 'end_framed_counter')
          and not is_profile_framed_counter(m) )
  then
    INTERNAL_ERR("metric named '"..name.."' did not have expected type "..
                 metric_str)
  end
  return m
end
local function check_metric_type( metric_str )
  return valid_metric_types[metric_str]
end

function Profiler:hook( handle, metric_name, metric_type, arg )
  if PROFILING_OFF then return quote end end
  if not check_metric_type(metric_type) then
    INTERNAL_ERR("could not find profiling metric type: "..metric_type) end
  local m               = get_metric(self, metric_name, metric_type)
  if      metric_type == 'counter' then
    return m:increment(`handle._id)
  elseif  metric_type == 'report' then
    return m:report((`handle._id), arg)
  elseif  metric_type == 'timer_start' then
    return m:start(`handle._id)
  elseif  metric_type == 'timer_stop' then
    return m:stop(`handle._id)
  elseif  metric_type == 'framed_counter' then
    return m:increment(`handle._id)
  elseif  metric_type == 'end_framed_counter' then
    return m:end_frame(`handle._id)
  else INTERNAL_ERR("impossible") end
end

function Profiler:get_output( handle )
  self:complete()
  return `handle:get_output()
end
function Profiler:get_output_struct()
  self:complete()
  return self._out_struct
end

function Profiler:print_profile( handle )
  local max_name_len    = #("(framed counts)")
  for _,m in ipairs(self._metrics) do
    if #m:name() > max_name_len then max_name_len = #m:name() end
  end
  local MNL             = tostring(max_name_len)
  local h_len           = MNL+3+10+3+11*3+2+10
  if PROFILING_OFF then
    return quote
      C.printf("   ---------------------------------------- \n")
      C.printf("  | No Profile.  Profiling was turned off. |\n")
      C.printf("   ---------------------------------------- \n")
    end
  end
  return quote
    var out             = [ self:get_output(handle) ]
    C.printf(["  %"..MNL.."s | \n"],
             "metric name", "count", "avg", "min", "max", "total time")
    C.printf(["  %"..MNL.."s | %10s | %10s %10s %10s   %10s\n"],
             "(raw counts)", "count", "-", "-", "-", "-")
    C.printf(["  %"..MNL.."s | %10s | %10s %10s %10s   %10s\n"],
             "(raw reports)", "n_reports", "avg", "min", "max", "sum")
    C.printf(["  %"..MNL.."s | %10s | %10s %10s %10s | %10s\n"],
             "(timers in ms)", "count", "avg", "min", "max", "total time")
    C.printf(["  %"..MNL.."s | %10s | %10s %10s %10s | %10s\n"],
             "(framed counts)", "n_frames", "avg", "min", "max", "sum count")
    C.printf(["  "..string.rep('-',h_len).."\n"])
    escape for _,m in ipairs(self._metrics) do
      local nm          = m:name()
      local pstmt       = nil
      local ptest       = nil
      if is_profile_counter(m) then
        pstmt = quote
          C.printf(["  %"..MNL.."s | %10d | %10s %10s %10s   %10s\n"],
                   nm, out.[nm].count, "-", "-", "-", "-")
        end
        ptest = (`true)
      elseif is_profile_report(m) then
        pstmt = quote
          C.printf(["  %"..MNL.."s | %10d | %10.3f %10.3f %10.3f | %10.3f\n"],
                   nm, out.[nm].n_reports,
                   out.[nm].avg, out.[nm].min, out.[nm].max,
                   out.[nm].sum)
        end
        ptest = (`out.[nm].n_reports ~= 0)
      elseif is_profile_elapsed(m) then
        pstmt = quote
          C.printf(["  %"..MNL.."s | %10d | %10.5f %10.5f %10.5f | %10.3f\n"],
                   nm, out.[nm].count,
                   out.[nm].avg*1e3, out.[nm].min*1e3, out.[nm].max*1e3,
                   out.[nm].sum*1e3)
        end
        ptest = (`out.[nm].count ~= 0)
      elseif is_profile_framed_counter(m) then
        pstmt = quote
          C.printf(["  %"..MNL.."s | %10d | %10.3f %10.0f %10.0f | %10.0f\n"],
                   nm, out.[nm].n_frames,
                   out.[nm].avg, out.[nm].min, out.[nm].max, out.[nm].sum)
        end
        ptest = (`out.[nm].n_frames ~= 0)
      else
        INTERNAL_ERR('unexpected metric type')
      end
      emit quote if [ptest] then [pstmt] end end
    end end
  end
end

----------------------------

Exports.NewProfiler     = NewProfiler
Exports.is_profiler     = is_profiler



