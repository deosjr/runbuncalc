#!/usr/bin/env python3
"""Recover a turn log from a recorded Run & Bun battle.

    python3 tools/battlelog.py fight.mp4 > fight.tsv

Needs ffmpeg and tesseract. Assumes a 640x360 recording of the GBA screen
letterboxed to x=70..570 (the layout YouTube's 360p gives); other sizes need
the crop boxes below adjusted. Output: one row per finished text-box message
with time, their HP bar fraction, our exact HP (read by glyph templates from
tools/hp_digit_templates.npy) and the message with words snapped to the
battle vocabulary. Their HP is a 48-pixel bar, so it is a fraction, not a
number; ours is printed as numerals, so it is exact.
"""
import subprocess, numpy as np, difflib, re, sys, os
VIDEO=sys.argv[1]; FPS=4
HERE=os.path.dirname(os.path.abspath(__file__))
TEXT="480:82:80:258"; TEXT_OCR="470:70:85:264"; HPBOX="66:18:470:214"; THEIRBAR="103:4:177:84"
VOCAB="""Kubfu Lopunny Combusken Hitmontop Poliwhirl Scraggy Foe used fainted sent out Leader Brawly Intimidate cuts Attack
Speed Boost raised its Speed critical hit super effective not very effective Oran Berry restored health Flame Body burned
hurt by its burn Eject Button switched with the enough Come back Go flinched paralyzed missed avoided attack gained Exp Points
What will do Choose Pokemon badly poisoned asleep woke fell confused snapped drained energy rose harshly sharply lowered
Brick Break Mega Punch Zen Headbutt Sucker Punch Retaliate Headbutt Drain Punch Double Kick Incinerate Thunder Punch Work Up
Mach Punch Rock Slide Fake Out Pursuit Bubble Beam Ice Beam Hidden Power Superpower Feint Attack Power-Up Punch Rock Tomb Rest
Quick Attack Aerial Ace Aqua Jet Brine Bite Low Sweep Mach Punch Wake-Up Slap Poison Fang Absorb Flame Wheel Glare Crunch""".split()
if len(sys.argv)>2: VOCAB+=open(sys.argv[2]).read().split()   # extra names: your party, its moves
def run(args): return subprocess.run(args,capture_output=True).stdout
def frame(t,crop,fmt="gray"): 
    w,h=[int(x) for x in crop.split(":")[:2]]
    raw=run(["ffmpeg","-v","error","-ss",str(t),"-i",VIDEO,"-frames:v","1","-vf",f"crop={crop},format={fmt}","-f","rawvideo","-"])
    return np.frombuffer(raw,dtype=np.uint8).reshape(h,w,-1).squeeze()
# 1. finished messages: the text box was still for a step and is about to change
W,H=480,82
raw=run(["ffmpeg","-v","error","-i",VIDEO,"-vf",f"fps={FPS},crop={TEXT},format=gray","-f","rawvideo","-"])
n=len(raw)//(W*H); a=np.frombuffer(raw,dtype=np.uint8)[:n*W*H].reshape(n,H,W)
d=np.abs(a[1:].astype(int)-a[:-1].astype(int)).mean(axis=(1,2))
ends=[i for i in range(1,n-1) if d[i]>1.0 and d[i-1]<0.5 and (a[i]>170).sum()>150]
kept=[]
for i in ends:
    if kept and np.abs(a[i].astype(int)-a[kept[-1]].astype(int)).mean()<0.5: continue
    kept.append(i)
# 2. our HP numerals by glyph template
templ=dict(zip("0123456789/",np.load(os.path.join(HERE,"hp_digit_templates.npy"))))
def glyphs(img):
    img=img[:16,:60]; ink=img<150; cols=ink.any(axis=0); out=[]; i=0
    while i<60:
        if cols[i]:
            j=i
            while j<60 and cols[j]: j+=1
            g=ink[:,i:j]; rows=np.where(g.any(axis=1))[0]; g=g[rows[0]:rows[-1]+1]
            ys=np.linspace(0,g.shape[0]-1,12).astype(int); xs=np.linspace(0,g.shape[1]-1,8).astype(int)
            out.append(g[ys][:,xs].astype(float)); i=j
        else: i+=1
    return out
def our_hp(t):
    s="".join(min(templ,key=lambda c:((templ[c]-g)**2).sum()) for g in glyphs(frame(t,HPBOX)))
    return s if re.fullmatch(r"\d{1,2}/\d{2}",s) else "?"+s
def their_bar(t):
    row=frame(t,THEIRBAR,"rgb24").astype(int)[2]; return (row.max(axis=1)>120).sum()/row.shape[0]
def fix(w):
    c=re.sub(r"[^A-Za-z'\-]","",w)
    if len(c)<4: return w
    m=difflib.get_close_matches(c,VOCAB,n=1,cutoff=0.7); return m[0] if m else w
TMP=os.path.join(os.path.dirname(os.path.abspath(VIDEO)),".battlelog_text.png")
def message(t):
    subprocess.run(["ffmpeg","-v","error","-ss",str(t),"-i",VIDEO,"-frames:v","1","-vf",
        f"crop={TEXT_OCR},scale=iw*4:ih*4:flags=neighbor,format=gray,lutyuv=y='if(gt(val,150),0,255)'","-y",TMP],check=True)
    r=subprocess.run(["tesseract",TMP,"-","--psm","6"],capture_output=True)
    return " ".join(fix(w) for w in r.stdout.decode("utf-8","replace").split())
print("time\ttheir_bar\tour_hp\tmessage")
for i in kept:
    t=i/FPS
    print(f"{t:.2f}\t{their_bar(t):.2f}\t{our_hp(t)}\t{message(t)}")
