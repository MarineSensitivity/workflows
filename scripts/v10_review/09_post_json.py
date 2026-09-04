# 09 - embed the simplified geometry into v9_data.json and fit the richness slope beta (log total ~ log n species)
# per category (the "optimal alpha" panel).
import json, math
import os; S = os.environ['S']
d=json.load(open(f'{S}/v9_data.json'))
pa=json.load(open(f'{S}/pa_shift.geojson')); usa=json.load(open(f'{S}/usa_shift.geojson'))
d['geo']={'pa':[{'pa':f['properties']['pa'],'g':f['geometry']} for f in pa['features']], 'usa':[f['geometry'] for f in usa['features']]}
cats=['turtle','mammal','bird','fish','invertebrate','coral','primary_producer']
def ols(x,y):
    n=len(x); mx=sum(x)/n; my=sum(y)/n; sxx=sum((a-mx)**2 for a in x); sxy=sum((a-mx)*(b-my) for a,b in zip(x,y)); b=sxy/sxx; a=my-b*mx
    ss=sum((b_-(a+b*a_))**2 for a_,b_ in zip(x,y)); st=sum((b_-my)**2 for b_ in y); return b, 1-ss/st if st else 0
d['alpha_star']={}
for c in cats:
    rows=[r for r in d['tab'] if r['cat']==c and r['current'] is not None and r['S_gl']>0 and r['n_present']>0]
    b,r2=ols([math.log10(r['n_present']) for r in rows],[math.log10(r['S_gl']) for r in rows]); d['alpha_star'][c]=dict(beta=round(b,3), r2=round(r2,3), n=len(rows))
json.dump(d, open(f'{S}/v9_data.json','w'), separators=(',',':'))
print('post: geo', len(d['geo']['pa']), 'study_area', round(d.get('study_area_km2',0)), 'pp mean', round(d.get('primprod_usa_mean',0),1), 'S_gl_usa', {c['cat']: round(c.get('S_gl_usa',0),3) for c in d['cats']})
