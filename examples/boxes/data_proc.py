#!/usr/bin/env python3

from collections import namedtuple
import numpy as np

import matplotlib
matplotlib.use('Agg')
from matplotlib import pyplot as plt

import math

plt.rcParams['figure.figsize'] = 6.6,4.5
#plt.rcParams['font.size'] = 10.0



class TimeStats:
  def __init__(self, avg, min, max):
    self.avg  = avg
    self.min  = min
    self.max  = max

  def __str__(self):
    return "(avg: %f, min: %f, max: %f)" %(self.avg, self.min, self.max)

class SumStats:
  def __init__(self, xs):
    xs.sort()
    N         = len(xs)
    assert N > 0
    self.avg  = sum(xs)/len(xs)
    self.med  = 0.5*(xs[ N // 2 ] + xs[ (N-1) // 2 ])
    self.min  = min(xs)
    self.max  = max(xs)

  def __str__(self):
    return ("(avg: %f, med: %f min: %f, max: %f)"%
            (self.avg, self.med, self.min, self.max))

def read_gong_boxes(filename):
  in_report         = 0
  in_collide        = False

  col_times         = None
  n_contacts        = []

  with open(filename,"r") as F:
    for line in F.readlines():
      if line.startswith("*** Final Report ***"):
        in_report   = 4
      elif line.startswith("*** n contacts ***"):
        in_collide  = True
      elif in_report > 0:
        in_report  -= 1
        if line.startswith("      collision"):
          assert col_times is None
          header,avg,lo,hi,div,total = line.split()
          col_times = TimeStats(float(avg),float(lo),float(hi))
      elif in_collide and (len(line.split()) == 2 and
                           line.split()[0][-1] == ":"):
        i,n         = line.split()
        i,n         = int(i[:-1]), int(n)
        assert len(n_contacts) == i
        n_contacts.append(n)
      else:
        in_collide  = False

  assert col_times is not None

  return col_times, n_contacts

def read_bullet_boxes(filename):
  in_report         = 0
  in_collide        = False

  col_times         = None
  n_contacts        = []

  with open(filename,"r") as F:
    for line in F.readlines():
      if line.startswith("*** Final Report ***"):
        in_report   = 8
      elif line.startswith("  collision graph counts"):
        in_collide  = True
      elif in_report > 0:
        in_report  -= 1
        if line.startswith("  timings(ms) - collision"):
          assert col_times is None
          h0,h1,h2, avg,lo,hi, d0,d1 = line.split()
          col_times = TimeStats(float(avg),float(lo),float(hi))
      elif in_collide and (len(line.split()) == 2):
        i,n         = line.split()
        i,n         = int(i), int(n)
        assert len(n_contacts) == i
        n_contacts.append(n)
      else:
        in_collide  = False

  assert col_times is not None

  return col_times, n_contacts


#def read_gong_stats(N, filename_pattern):
#  # bullet boxes
#  all_times = []
#  all_n_contacts = []
#  for i in range(0,N):
#    ts, n_contacts = read_gong_boxes(filename_pattern % i)
#    all_times.append(ts)
#    all_n_contacts.append(n_contacts)
#
#  all_n_contacts  = [ SumStats(list(x)) for x in zip(*all_n_contacts) ]
#  all_times       = SumStats([ x.avg for x in all_times ])
#  return all_times, all_n_contacts
#
#def read_bullet_stats(N, filename_pattern):
#  # bullet boxes
#  bullet_times = []
#  bullet_n_contacts = []
#  for i in range(0,N):
#    ts, n_contacts = read_bullet_boxes(filename_pattern % i)
#    bullet_times.append(ts)
#    bullet_n_contacts.append(n_contacts)
#
#  bullet_n_contacts = [ SumStats(list(x)) for x in zip(*bullet_n_contacts) ]
#  bullet_times      = SumStats([ x.avg for x in bullet_times ])
#  return bullet_times, bullet_n_contacts


#ax.plot(a, c, 'k--', label='Model length')
#ax.plot(a, d, 'k:', label='Data length')
#ax.plot(a, c + d, 'k', label='Total message length')

#legend = ax.legend(loc='upper center', shadow=True, fontsize='x-large')

# init dictionary hierarchy
times = {}
for platform in ['bullet','gong_cpu_scan','gong_cpu_bvh','gong_cpu_hash',
                          'gong_gpu_scan','gong_gpu_bvh']:
  times[platform] = []


def do_compare(exp_name,N):
  fig, ax   = plt.subplots()

  exp_name  = "round_tower_out_" + exp_name
  b_name    = "exp_out/bullet_blocks_out/" + exp_name + ".txt"
  g_cpu_scan= "exp_out/gong_cpu_scan/" + exp_name + ".txt"
  g_cpu_bvh = "exp_out/gong_cpu_bvh/" + exp_name + ".txt"
  g_cpu_hash= "exp_out/gong_cpu_hash/" + exp_name + ".txt"
  g_gpu_scan= "exp_out/gong_gpu_scan/" + exp_name + ".txt"
  g_gpu_bvh = "exp_out/gong_gpu_bvh/" + exp_name + ".txt"

  ts, n_contacts = read_bullet_boxes(b_name)
  ax.plot(range(0,len(n_contacts)),n_contacts,label="Bullet (cpu)")
  print('bullet (cpu) times', ts)
  times['bullet'].append(ts.avg)

  ts, n_contacts = read_gong_boxes(g_cpu_scan)
  ax.plot(range(0,len(n_contacts)),n_contacts,label="Gong (cpu,scan)")
  print('gong (cpu,scan) times', ts)
  times['gong_cpu_scan'].append(ts.avg)

  ts, n_contacts = read_gong_boxes(g_cpu_bvh)
  ax.plot(range(0,len(n_contacts)),n_contacts,label="Gong (cpu,bvh)")
  print('gong (cpu,bvh) times', ts)
  times['gong_cpu_bvh'].append(ts.avg)

  ts, n_contacts = read_gong_boxes(g_cpu_hash)
  ax.plot(range(0,len(n_contacts)),n_contacts,label="Gong (cpu,hash)")
  print('gong (cpu,hash) times', ts)
  times['gong_cpu_hash'].append(ts.avg)

  ts, n_contacts = read_gong_boxes(g_gpu_scan)
  ax.plot(range(0,len(n_contacts)),n_contacts,label="Gong (gpu,scan)")
  print('gong (gpu,scan) times', ts)
  times['gong_gpu_scan'].append(ts.avg)

  ts, n_contacts = read_gong_boxes(g_gpu_bvh)
  ax.plot(range(0,len(n_contacts)),n_contacts,label="Gong (gpu,bvh)")
  print('gong (gpu,bvh) times', ts)
  times['gong_gpu_bvh'].append(ts.avg)

  ax.legend()
  plt.savefig("exp_out/"+exp_name+"_n_contacts.pdf")
  plt.close(fig)


do_compare("h25_r24",600)
do_compare("h50_r24",1200)
do_compare("h50_r48",2400)
do_compare("h100_r48",4800)
do_compare("h100_r96",9600)
do_compare("h200_r96",19200)
do_compare("h200_r192",38400)
sizes = [600,1200,2400,4800,9600,19200,38400]

fig, ax = plt.subplots()
ax.plot(sizes,times['bullet'],label="Bullet (cpu)",
              marker='o', color='tab:blue')
ax.plot(sizes,times['gong_cpu_scan'],label="Gong (cpu,scan)",
              marker='>', color='tab:orange')
ax.plot(sizes,times['gong_cpu_bvh'],label="Gong (cpu,bvh)",
              marker='v', color='tab:orange')
ax.plot(sizes,times['gong_cpu_hash'],label="Gong (cpu,hash)",
              marker='^', color='tab:orange')
ax.plot(sizes,times['gong_gpu_scan'],label="Gong (gpu,scan)",
              marker='x', color='tab:green')
ax.plot(sizes,times['gong_gpu_bvh'],label="Gong (gpu,bvh)",
              marker='+', color='tab:green')
ax.set_yscale('log')
ax.set_xscale('log')
ax.set_xticks(sizes)
ax.set_xticklabels(sizes)
ax.set_xlim(left=500,right=45000)
ax.set_ylim(ymin=0.5,ymax=200)
ax.set_xlabel("# of boxes")
ax.set_ylabel("ms per frame")
ax.legend(loc="upper left")
fig.tight_layout()
plt.savefig("exp_out/round_tower_times.pdf")
plt.close(fig)













# -----
# -----
# Cork Data

CORK_DATUM = namedtuple('CORK_DATUM',
                        ['label','name','n_edge','n_tri',
                         'cork_ms','gong_cpu_ms','gong_gpu_ms'])
cork_data = [
CORK_DATUM('a', 'HexPentaPot',             11532,    7688,  117,  118,  65),
CORK_DATUM('b', 'FourInARowV2',            35292,   23528,  142,  146,  54),
CORK_DATUM('c', 'pcb_vise_v2-Swivel',      36468,   24312,  109,  106, 184),
CORK_DATUM('d', 'Wizard_Hat',              44502,   29668,  144,  152,  88),
CORK_DATUM('e', 'Shamrock_Shot',           57870,   38580,  181,  180,  56),
CORK_DATUM('f', 'octopus',                 61026,   40684,  115,  118,  33),
CORK_DATUM('g', 'stylo2TOM',               77370,   51580,  261,  227,  49),
CORK_DATUM('h', 'soapPumpRotor',           78108,   52072,  153,  143,  48),
CORK_DATUM('i', 'dual-dodec',             104400,   69600,  222,  190,  23),
CORK_DATUM('j', 'inverted-bracelet',      179520,  119680,  457,  436, 159),
CORK_DATUM('k', 'metatron',               415140,  276760,  878,  783, 198),
CORK_DATUM('l', 'GOYLE_LOW',              589824,  393216,  961,  917, 172),
CORK_DATUM('m', 'TestForms01a-Final01b', 2264064, 1509376, 4021, 3914, 675),
]

fig, (ax,ax2) = plt.subplots(1,2,sharey=True)

y_pos = np.arange(len(cork_data))  # the label locations
width = 0.25  # the width of the bars

colors = ['tab:blue','tab:orange','tab:green']

cork_ms = [d.cork_ms for d in cork_data]
cpu_ms  = [d.gong_cpu_ms for d in cork_data]
gpu_ms  = [d.gong_gpu_ms for d in cork_data]
bars0 = ax.barh(y_pos-width,cork_ms,width, label="Cork",
                color=colors[0])
bars2 = ax2.barh(y_pos-width,cork_ms,width, label="Cork",
                 color=colors[0])
ax.barh(y_pos,cpu_ms, width,   label="Gong (cpu)", color=colors[1])
ax2.barh(y_pos,cpu_ms, width,   label="Gong (cpu)", color=colors[1])
ax.barh(y_pos+width,gpu_ms, width,   label="Gong (gpu)", color=colors[2])
ax2.barh(y_pos+width,gpu_ms, width,   label="Gong (gpu)", color=colors[2])
ax.set_yticks(y_pos)
ax.set_yticklabels(['('+d.label+')' for d in cork_data])
ax.invert_yaxis()
ax.set_xticks(200*np.arange(5))
ax2.set_xticks(200*np.arange(5)+3600)
ax.set_xlim(0,1000)
ax2.set_xlim(3500,4500)
ax.tick_params(axis='y', which='both',length=0)
ax2.tick_params(axis='y', which='both',length=0)
ax.set_xlabel("time (ms)")
ax.spines['right'].set_visible(False)
ax2.spines['left'].set_visible(False)
for bar,d in zip(bars0,cork_data):
  speedup = d.cork_ms / d.gong_gpu_ms
  width   = bar.get_width()
  ax.annotate("%.1fx" % speedup,
              xy=(width, bar.get_y()+bar.get_height()/2),
              xytext=(3,0),
              textcoords="offset points",
              ha='left',va='center')
  ax2.annotate("%.1fx" % speedup,
              xy=(width, bar.get_y()+bar.get_height()/2),
              xytext=(3,0),
              textcoords="offset points",
              ha='left',va='center')
ax2.legend()
fig.tight_layout()
plt.savefig("exp_out/cork_times.pdf")
plt.close(fig)





