# Draft email to Tim (2026-10-02)

HELD by Ben (2026-10-02): the Atlas "ready for review" email goes first. Before sending this one: attach the
September AWS bill PDF by hand, and revisit Recommendation 2 — Ben decided the same day to KEEP AquaX until the
OBIS-SDM replacements exist, so that paragraph should become "keep the previews; AquaX stays until OBIS models
replace it, and here is why its files cannot be kept private meanwhile" rather than "remove it now".
The fix described in the second paragraph is done and verified (disk 11G free at 10:10 UTC, key disabled 10:18).
A copy is in Gmail drafts (ben@ecoquants.com, cc ben@oceanmetrics.io).

**To:** Timothy.White@boem.gov
**Subject:** BOEM internal MST server: what went wrong, and a simpler way forward (plus AquaX)

Hi Tim,

Three things: a problem I found and fixed today, a recommendation to retire the internal BOEM copy of the
toolkit, and a recommendation on AquaX. Happy to walk through any of it at our next check-in.

**What happened.** Since February, the BOEM internal server (ioemazeudmar01) has been set up to fetch updates
from our cloud server every hour, so BOEM would have its own copy of the apps inside the network (the design is
written up at https://marinesensitivity.org/server/sync.html). On August 27
its disk filled up. From then on it asked for the same 3,950 map files every hour, could not save them, and
asked again the next hour. Nobody was alerted on either side. The cost landed on my cloud bill as "data
transfer" charges: about $475 from late August through today, on a line that is normally close to zero (the
September bill is attached). The apps on the internal server are also not running at the moment.

**What I did.** I changed our side so the hourly request has nothing left to re-download. The transfers stopped
at the next hourly check, and the internal server cleared about 11 GB of copies it never needed, so its disk
is no longer full. I am asking Amazon for a credit and adding
billing alarms so a runaway like this is caught within a day. Nothing is needed from BOEM IT urgently.

**Recommendation 1: retire the internal copy.** We built it when the tools were Shiny apps, which need a
server running R around the clock, and BOEM wanted that server inside its network. The new Atlas works
differently. It is a web page, hosted the same way as the documentation site, that reads data files directly
from cloud storage. The files are organized so a browser fetches only the small piece it needs for the map on
screen. There is no application server to run, patch, secure or keep in sync. The internal copy now duplicates
an older version (v4) of tools we are replacing. If BOEM later wants its own copy, it would be a folder of
files, which is a much smaller request to IT than a server. If you agree, could you point me to whoever in IT
should switch off the hourly jobs on that machine? I will then close its access on our end.

**Recommendation 2: keep the previews, skip AquaX.** We still need the sign-in preview site, and it stays. It
is how you, I and the data providers look at a release under development and check each new dataset before
anything is public. That is a light gate: it only has to keep unfinished work out of public view, and it does.
Keeping a provider's source files private is a different and much heavier requirement. Everything the Atlas
draws has to exist as a file in cloud storage, and the sign-in controls who sees the app, not who can download
a file if they have its address. Gabriel was clear that the AquaMaps 2.0 grids must stay private. I can't
promise his consortium that in good conscience without building and running a private server for one dataset,
which also runs against the open, reproducible spirit of the project. We already have the alternative: the
open OBIS models in the round 1 proposal I sent on September 16. I suggest we remove the AquaX layers from the
preview and from storage, keep our comparison with AquaX as an internal benchmark, and put the effort into the
OBIS route.

Does that sound right to you? If so I'll write to Gabriel, and we can take the internal server question to IT
together.

Cheers,
Ben
