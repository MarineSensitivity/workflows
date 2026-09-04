# 10 - inject v9_data.json into the template, make the document pure ASCII (entities in markup, \u escapes in
# script/JSON, \XXXX in CSS: the artifact viewer showed mojibake for UTF-8), syntax-check the script with node,
# re-parse the embedded JSON. Output: sensitivity-across-regions.html beside this script (tracked).
import re, json, subprocess, sys
import os
S = os.environ['S']; here = os.path.dirname(os.path.abspath(__file__))
t = open(f'{here}/sensitivity-across-regions.tpl.html', encoding='utf-8').read()
d = open(f'{S}/v9_data.json', encoding='utf-8').read(); json.loads(d)
doc = t.replace('__DATA_JSON__', d.replace('</script', '<\\/script'))
if not doc.lstrip().startswith('<meta charset'):
    doc = '<meta charset="utf-8">\n' + doc
# make the whole document ASCII: entities in markup, \u escapes in script/JSON, \XXXX in CSS
def esc_html(s): return re.sub(r'[^\x00-\x7f]', lambda m: '&#%d;' % ord(m.group()), s)
def esc_js(s):   return re.sub(r'[^\x00-\x7f]', lambda m: '\\u%04x' % ord(m.group()), s)
def esc_css(s):  return re.sub(r'[^\x00-\x7f]', lambda m: '\\%04x ' % ord(m.group()), s)
out, pos = [], 0
for m in re.finditer(r'<(style|script)\b[^>]*>.*?</\1>', doc, flags=re.S):
    out.append(esc_html(doc[pos:m.start()]))
    seg = m.group(0); open_tag = re.match(r'<[^>]*>', seg).group(0); close = '</%s>' % m.group(1)
    body = seg[len(open_tag):-len(close)]
    out.append(open_tag + (esc_css(body) if m.group(1) == 'style' else esc_js(body)) + close)
    pos = m.end()
out.append(esc_html(doc[pos:]))
final = ''.join(out)
assert all(ord(c) < 128 for c in final), 'non-ascii remains'
open(f'{here}/sensitivity-across-regions.html', 'w', encoding='ascii').write(final)
js = re.findall(r'<script>(.*?)</script>', final, flags=re.S)[-1]
open(f'{S}/page_script.js', 'w').write(js)
r = subprocess.run(['node', '--check', f'{S}/page_script.js'], capture_output=True, text=True)
print('node --check exit', r.returncode, r.stderr[:300]); print('bytes', len(final))
# the JSON block must still parse after escaping
jd = re.search(r'<script id="v9data" type="application/json">(.*?)</script>', final, flags=re.S).group(1)
json.loads(jd.replace('<\\/script', '</script>')); print('json ok')
