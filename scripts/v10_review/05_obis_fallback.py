# 05 - the R2 fallback fraction (share of the species' global distribution inside US waters) for the
# national-only taxa of 04: endemic (1.0) or a literature fraction where known, otherwise the OBIS
# occurrence fraction (records in OBIS area 266 "United States: all" / worldwide; floor 0.02; effort-biased).
# Writes tmp/us_share_fallback.csv and compares it with the committed data/us_share_fallback.csv;
# OBIS_WRITE_DATA=1 overwrites the committed file (diff it before committing).
import json, os, csv, time, urllib.request
S = os.environ['S']; repo = os.environ.get('MS_REPO', '.')
rows = list(csv.DictReader(open(f'{S}/v9_national_only.csv')))

def get(url):
    err = None
    for _ in range(3):
        try:
            req = urllib.request.Request(url, headers={'User-Agent': 'msens-review/1.0'})
            with urllib.request.urlopen(req, timeout=60) as r: return json.loads(r.read().decode())
        except Exception as e:
            err = e; time.sleep(2)
    raise err

counts_path = f'{S}/obis_counts.json'
if os.environ.get('REDO_OBIS') == '1' or not os.path.exists(counts_path):
    areas = get('https://api.obis.org/v3/area'); res = areas.get('results', areas) if isinstance(areas, dict) else areas
    us_all = [a for a in res if a.get('name') == 'United States: all']
    assert us_all and str(us_all[0]['id']) == '266', us_all
    out = []
    for r in rows:
        tid = r['taxon_id']
        tot = get(f'https://api.obis.org/v3/occurrence?taxonid={tid}&size=0').get('total', 0)
        us  = get(f'https://api.obis.org/v3/occurrence?taxonid={tid}&areaid=266&size=0').get('total', 0)
        out.append(dict(taxon_id=tid, sci=r['scientific_name'], common=r['common_name'], ds=r['ds'], sp_cat=r['sp_cat'],
                        obis_total=tot, obis_us=us, obis_frac=(us / tot if tot else None)))
        print(f"{r['scientific_name']:45s} {r['ds']:28s} OBIS total {tot:7d}  US {us:6d}")
    json.dump(out, open(counts_path, 'w'), indent=1)
obis = {r['taxon_id']: r for r in json.load(open(counts_path))}

# curated overrides (taxon_id = WoRMS AphiaID): endemics and literature fractions
endemic = {
 '159131': 'Hawaiian endemic subspecies (USFWS ESA listing, Hawaiian stilt recovery plan)',
 '1805509': 'US-endemic subspecies, Gulf of Mexico rivers and coast (USFWS/NMFS listing)',
 '280737': 'California endemic (USFWS listing)',
 '276031': 'northern Gulf of Mexico endemic, Texas to Florida (NatureServe, FWS range)',
 '515065': 'Hawaiian anchialine-pool endemic (USFWS)',
 '514615': 'Hawaiian anchialine-pool endemic (USFWS)',
 '514157': 'Hawaiian anchialine-pool endemic (USFWS ESA listing)',
 '514158': 'Hawaiian anchialine-pool endemic (USFWS ESA listing)',
 '242601': 'California endemic subspecies (USFWS stock assessment)',
}
literature = {
 '242600': (0.90, 'USFWS stock assessments: roughly 100,000 of 110,000 northern sea otters in Alaska and Washington, ~8,000 in British Columbia'),
 '159074': (0.50, 'BirdLife/Partners in Flight: breeds on US Atlantic and Gulf coasts, Mexico, the Caribbean and northern South America; about half the population in the US. The real fix is the name crosswalk (BOTW: Leucophaeus atricilla), which would give it a global range'),
 '159038': (0.25, 'presumed extinct (last confirmed 1963); historic breeding range in Canada, migration through the US; flagged'),
}
out = []
for r in rows:
    tid = r['taxon_id']; o = obis[tid]
    if tid in endemic: frac, method, basis = 1.0, 'endemic', endemic[tid]
    elif tid in literature: frac, method, basis = literature[tid][0], 'literature', literature[tid][1]
    else:
        f = o['obis_frac'] if o['obis_frac'] is not None else 0
        frac = max(f, 0.02); conf = 'low' if (o['obis_total'] < 100 or o['obis_us'] == 0) else 'moderate'
        method = 'obis'
        basis = (f"OBIS records in the US EEZ / worldwide: {o['obis_us']:,} / {o['obis_total']:,}"
                 + (" (floor of 0.02 applied)" if f < 0.02 else "") + f"; effort-biased, {conf} confidence")
    out.append(dict(taxon_id=tid, scientific_name=r['scientific_name'], common_name=r['common_name'], sp_cat=r['sp_cat'], ds=r['ds'],
                    fraction=round(frac, 3), method=method, basis=basis, obis_total=o['obis_total'], obis_us=o['obis_us']))
tmp_csv = f'{S}/us_share_fallback.csv'
with open(tmp_csv, 'w', newline='') as f:
    w = csv.DictWriter(f, fieldnames=list(out[0].keys())); w.writeheader(); w.writerows(out)
data_csv = f'{repo}/data/us_share_fallback.csv'
same = os.path.exists(data_csv) and open(data_csv).read() == open(tmp_csv).read()
print(f'fallback rows: {len(out)}; identical to data/us_share_fallback.csv: {same}')
if os.environ.get('OBIS_WRITE_DATA') == '1':
    open(data_csv, 'w').write(open(tmp_csv).read()); print('wrote', data_csv)
elif not same:
    print('NOTE: tmp/us_share_fallback.csv differs from data/ (OBIS counts move); rerun with OBIS_WRITE_DATA=1 to adopt it')
