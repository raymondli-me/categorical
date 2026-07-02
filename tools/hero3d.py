import os, numpy as np, pandas as pd
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
from scipy.spatial import ConvexHull

D=os.environ.get("FIGDIR",".")
seg=pd.read_csv(f"{D}/fig3d_seg.csv"); meta=pd.read_csv(f"{D}/fig3d_meta.csv"); pct=meta["benz"].tolist()
P=seg[["d1","d2","d3"]].values; lab=seg["clu"].values
COLOR={0:"#1b9e77",1:"#2c7fb8",2:"#d95f02",3:"#7570b3"}

fig=plt.figure(figsize=(9,8),dpi=200)
ax=fig.add_axes([0.02,0.02,0.96,0.96],projection="3d")
for c in sorted(set(lab)):
    pts=P[lab==c]
    ax.scatter(pts[:,0],pts[:,1],pts[:,2],s=7,color=COLOR[c],edgecolors="none",alpha=0.30,zorder=1)
    try:
        poly=Poly3DCollection([pts[s] for s in ConvexHull(pts).simplices],
             alpha=0.12,facecolor=COLOR[c],edgecolor=COLOR[c],linewidths=0.2)
        poly.set_zorder(2); ax.add_collection3d(poly)
    except Exception: pass
ax.view_init(elev=20,azim=-58)
for pane in (ax.xaxis,ax.yaxis,ax.zaxis):
    pane.pane.set_facecolor("white"); pane.pane.set_edgecolor("#eee"); pane.pane.set_alpha(1.0)
    pane._axinfo["grid"]["color"]="#f5f5f5"; pane._axinfo["grid"]["linewidth"]=0.3
    pane.line.set_color("#cccccc"); pane.line.set_linewidth(0.7)
ax.set_xticklabels([]); ax.set_yticklabels([]); ax.set_zticklabels([])
ax.tick_params(length=0)
try: ax.set_box_aspect((1,1,0.85))
except Exception: pass
fig.savefig(f"{D}/titanic-mca-3d.png",bbox_inches="tight",pad_inches=0.1)
print("saved titanic-mca-3d.png")
