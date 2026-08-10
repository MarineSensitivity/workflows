"""Build the v6 revision: apply tracked-change edits + threaded comment replies
to the reviewed docx, writing a new .docx (original untouched)."""
import copy, os, shutil, sys, tempfile, zipfile
from lxml import etree

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import importlib
sys.modules["final_report_2025_review_responses_content"] = importlib.machinery.SourceFileLoader(
    "final_report_2025_review_responses_content",
    os.path.join(os.path.dirname(os.path.abspath(__file__)), "final-report_2025_review-responses_content.py")).load_module()
from final_report_2025_review_responses_content import AUTHOR, INITIALS, DATE, REPLIES, EDITS, REFERENCES

SRC = os.path.expanduser("~/Desktop/BOEM-MarineSensitivityToolkit_2025-final-report_v5_MKR.docx")
OUT = os.path.expanduser("~/Desktop/BOEM-MarineSensitivityToolkit_2025-final-report_v6_BB.docx")
WORK = os.path.join(tempfile.gettempdir(), "msens_report_v6_build")

NS = {
  "w":      "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
  "w14":    "http://schemas.microsoft.com/office/word/2010/wordml",
  "w15":    "http://schemas.microsoft.com/office/word/2012/wordml",
  "w16cid": "http://schemas.microsoft.com/office/word/2016/wordml/cid",
  "w16cex": "http://schemas.microsoft.com/office/word/2018/wordml/cex",
}
W, W14, W15, W16CID, W16CEX = ("{%s}" % NS[k] for k in ("w", "w14", "w15", "w16cid", "w16cex"))

# --- unpack -----------------------------------------------------------------
if os.path.isdir(WORK):
    shutil.rmtree(WORK)
os.makedirs(WORK)
with zipfile.ZipFile(SRC) as z:
    names = z.namelist()
    z.extractall(WORK)

def load(rel):
    return etree.parse(os.path.join(WORK, rel))

doc   = load("word/document.xml")
cmts  = load("word/comments.xml")
cext  = load("word/commentsExtended.xml")
cids  = load("word/commentsIds.xml")
cexb  = load("word/commentsExtensible.xml")
ppl   = load("word/people.xml")

body = doc.getroot().find(W + "body")
paras = list(body.iter(W + "p"))          # 1-based indexing matches the extraction
def P(i):
    return paras[i - 1]

# --- id generators ----------------------------------------------------------
_change_id = [9000]
def change_id():
    _change_id[0] += 1
    return str(_change_id[0])

_hex = [0x0BB00000]
def new_hex():
    _hex[0] += 0x11
    return "%08X" % _hex[0]

# ---------------------------------------------------------------------------
# tracked-change helpers
# ---------------------------------------------------------------------------
def make_run(text, bold=False):
    r = etree.SubElement(etree.Element("tmp"), W + "r")
    if bold:
        rPr = etree.SubElement(r, W + "rPr")
        etree.SubElement(rPr, W + "b")
        etree.SubElement(rPr, W + "bCs")
    t = etree.SubElement(r, W + "t")
    t.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")
    t.text = text
    return r

def make_ins(runs):
    ins = etree.Element(W + "ins")
    ins.set(W + "id", change_id())
    ins.set(W + "author", AUTHOR)
    ins.set(W + "date", DATE)
    for r in runs:
        ins.append(r)
    return ins

def make_para(spec_runs, ppr_source=None, style=None):
    """spec_runs: [(text, bold), ...]. Paragraph mark is marked inserted."""
    p = etree.Element(W + "p")
    p.set(W14 + "paraId", new_hex())
    p.set(W14 + "textId", new_hex())
    if style is not None:
        pPr = etree.SubElement(p, W + "pPr")
        pStyle = etree.SubElement(pPr, W + "pStyle")
        pStyle.set(W + "val", style)
    elif ppr_source is not None:
        src = ppr_source.find(W + "pPr")
        pPr = copy.deepcopy(src) if src is not None else etree.SubElement(p, W + "pPr")
        if pPr.getparent() is None:
            p.append(pPr)
        # drop any rPr inherited from the source so we can set our own ins mark
        for old in pPr.findall(W + "rPr"):
            pPr.remove(old)
    else:
        pPr = etree.SubElement(p, W + "pPr")
    rPr = etree.SubElement(pPr, W + "rPr")
    mark = etree.SubElement(rPr, W + "ins")
    mark.set(W + "id", change_id())
    mark.set(W + "author", AUTHOR)
    mark.set(W + "date", DATE)
    p.append(make_ins([make_run(t, b) for t, b in spec_runs]))
    return p

def append_tracked(p, spec_runs):
    p.append(make_ins([make_run(t, b) for t, b in spec_runs]))

def replace_tracked(p, spec_runs):
    """Mark every existing run deleted, then append the replacement as inserted."""
    for r in list(p.findall(W + "r")):
        idx = list(p).index(r)
        d = etree.Element(W + "del")
        d.set(W + "id", change_id())
        d.set(W + "author", AUTHOR)
        d.set(W + "date", DATE)
        p.remove(r)
        for t in r.findall(W + "t"):
            t.tag = W + "delText"
            t.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")
        d.append(r)
        p.insert(idx, d)
    append_tracked(p, spec_runs)

def insert_after(anchor, new_elems):
    prev = anchor
    for e in new_elems:
        prev.addnext(e)
        prev = e

# ---------------------------------------------------------------------------
# 1. apply body edits
# ---------------------------------------------------------------------------
PPR_FROM = {146: 147, 153: 154, 169: 163, 282: 284, 228: 227}

applied = []
for kind, idx, payload in EDITS:
    p = P(idx)
    if kind == "append":
        append_tracked(p, payload)
    elif kind == "replace":
        assert p.find(W + "r") is not None, f"p{idx} has no runs"
        assert p.find(".//m:oMath", namespaces={"m": "http://schemas.openxmlformats.org/officeDocument/2006/math"}) is None, \
            f"p{idx} contains math; refusing to replace"
        replace_tracked(p, payload)
    elif kind == "after":
        src = P(PPR_FROM.get(idx, idx))
        new = []
        for style, runs in payload:
            if style and style.startswith("Heading"):
                new.append(make_para(runs, style=style))
            else:
                new.append(make_para(runs, ppr_source=src))
        insert_after(p, new)
    applied.append((kind, idx))

# ---------------------------------------------------------------------------
# 2. references (always Bibliography style), grouped per anchor, in order
# ---------------------------------------------------------------------------
from collections import defaultdict, OrderedDict
groups = OrderedDict()
for idx, text in REFERENCES:
    groups.setdefault(idx, []).append(text)
for idx, texts in groups.items():
    insert_after(P(idx), [make_para([(t, False)], style="Bibliography") for t in texts])

# ---------------------------------------------------------------------------
# 3. threaded replies to every reviewer comment
# ---------------------------------------------------------------------------
existing_ids = [int(c.get(W + "id")) for c in cmts.getroot().findall(W + "comment")]
next_id = max(existing_ids) + 1

# map parent comment id -> its paraId (needed for w15:paraIdParent)
parent_paraid = {}
for c in cmts.getroot().findall(W + "comment"):
    first_p = c.find(W + "p")
    parent_paraid[c.get(W + "id")] = first_p.get(W14 + "paraId")

def make_comment(cid, para_id, text):
    c = etree.SubElement(cmts.getroot(), W + "comment")
    c.set(W + "id", cid)
    c.set(W + "author", AUTHOR)
    c.set(W + "date", DATE)
    c.set(W + "initials", INITIALS)
    p = etree.SubElement(c, W + "p")
    p.set(W14 + "paraId", para_id)
    p.set(W14 + "textId", new_hex())
    pPr = etree.SubElement(p, W + "pPr")
    pStyle = etree.SubElement(pPr, W + "pStyle")
    pStyle.set(W + "val", "CommentText")
    r0 = etree.SubElement(p, W + "r")
    rPr = etree.SubElement(r0, W + "rPr")
    rStyle = etree.SubElement(rPr, W + "rStyle")
    rStyle.set(W + "val", "CommentReference")
    etree.SubElement(r0, W + "annotationRef")
    r1 = etree.SubElement(p, W + "r")
    t = etree.SubElement(r1, W + "t")
    t.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")
    t.text = text
    return c

# anchors in document.xml, keyed by parent comment id
starts = {e.get(W + "id"): e for e in body.iter(W + "commentRangeStart")}
ends   = {e.get(W + "id"): e for e in body.iter(W + "commentRangeEnd")}
refs   = {}
for rr in body.iter(W + "commentReference"):
    refs[rr.get(W + "id")] = rr.getparent()

n_replies = 0
for pid, text in REPLIES.items():
    if pid not in starts:
        print("  ! no anchor for parent comment", pid)
        continue
    cid = str(next_id); next_id += 1
    para_id = new_hex()
    durable = new_hex()

    make_comment(cid, para_id, text)

    ce = etree.SubElement(cext.getroot(), W15 + "commentEx")
    ce.set(W15 + "paraId", para_id)
    ce.set(W15 + "paraIdParent", parent_paraid[pid])
    ce.set(W15 + "done", "0")

    ci = etree.SubElement(cids.getroot(), W16CID + "commentId")
    ci.set(W16CID + "paraId", para_id)
    ci.set(W16CID + "durableId", durable)

    cx = etree.SubElement(cexb.getroot(), W16CEX + "commentExtensible")
    cx.set(W16CEX + "durableId", durable)
    cx.set(W16CEX + "dateUtc", DATE)

    # anchor the reply over the same range as its parent
    s = etree.Element(W + "commentRangeStart"); s.set(W + "id", cid)
    starts[pid].addprevious(s)
    e = etree.Element(W + "commentRangeEnd");   e.set(W + "id", cid)
    ends[pid].addnext(e)
    run = etree.Element(W + "r")
    rPr = etree.SubElement(run, W + "rPr")
    rStyle = etree.SubElement(rPr, W + "rStyle"); rStyle.set(W + "val", "CommentReference")
    cr = etree.SubElement(run, W + "commentReference"); cr.set(W + "id", cid)
    refs[pid].addnext(run)
    n_replies += 1

# people.xml: register the new author
person = etree.SubElement(ppl.getroot(), W15 + "person")
person.set(W15 + "author", AUTHOR)
pres = etree.SubElement(person, W15 + "presenceInfo")
pres.set(W15 + "providerId", "None")
pres.set(W15 + "userId", AUTHOR)

# ---------------------------------------------------------------------------
# 4. write back
# ---------------------------------------------------------------------------
for tree, rel in ((doc, "word/document.xml"), (cmts, "word/comments.xml"),
                  (cext, "word/commentsExtended.xml"), (cids, "word/commentsIds.xml"),
                  (cexb, "word/commentsExtensible.xml"), (ppl, "word/people.xml")):
    tree.write(os.path.join(WORK, rel), xml_declaration=True, encoding="UTF-8", standalone=True)

if os.path.exists(OUT):
    os.remove(OUT)
with zipfile.ZipFile(OUT, "w", zipfile.ZIP_DEFLATED) as z:
    for name in names:                      # preserve original entry order
        z.write(os.path.join(WORK, name), name)

print(f"edits applied : {len(applied)}")
print(f"references    : {sum(len(v) for v in groups.values())}")
print(f"replies       : {n_replies}")
print(f"wrote         : {OUT}  ({os.path.getsize(OUT)/1e6:.1f} MB)")
