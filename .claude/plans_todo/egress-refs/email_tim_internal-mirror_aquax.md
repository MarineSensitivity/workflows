# Draft email to Tim (2026-10-02)

STATUS (checked 2026-10-02 15:35 UTC): the Gmail copy is in SENT, dated 2026-10-02 15:25:19 UTC, with the
ORIGINAL text (Recommendation 2 = "skip AquaX ... remove the AquaX layers from the preview and from storage"; no
sync.html link, no bill attached). Not sent by the agent session. The text below is the REVISED one Ben asked for
(AquaX stays until the OBIS-SDM replacements exist; sync.html link; "bill attached") and was never sent. If the
send was not intended, the follow-up to Tim needs only: AquaX stays in the v9 preview for now, the bill PDF, and
the sync.html link.

**To:** Timothy.White@boem.gov
**Subject:** BOEM internal MST server: what went wrong, and a simpler way forward (plus a note on AquaX)

Hi Tim,

Three things: a problem I found and fixed today, a recommendation to retire the internal BOEM copy of the
toolkit, and where I think we should head with AquaX. Happy to walk through any of it at our next check-in.

**What happened.** Since February, the BOEM internal server (ioemazeudmar01) has been set up to fetch updates
from our cloud server every hour, so BOEM would have its own copy of the apps inside the network (the design is
written up at https://marinesensitivity.org/server/sync.html). On August 27
its disk filled up. From then on it asked for the same 3,950 map files every hour, could not save them, and
asked again the next hour. Nobody was alerted on either side. The cost landed on my cloud bill as "data
transfer" charges: about $470 from late August through today, on a line that is normally close to zero (the
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

**Recommendation 2: keep the previews, and plan to replace AquaX.** We still need the sign-in preview site,
and it stays. It is how you, I and the data providers look at a release under development and check each new
dataset before anything is public. That is a light gate: it only has to keep unfinished work out of public
view, and it does. Keeping a provider's source files private is a different and much heavier requirement.
Everything the Atlas draws has to exist as a file in cloud storage, and the sign-in controls who sees the app,
not who can download a file if they have its address. Gabriel was clear that the AquaMaps 2.0 grids must stay
private. I can't promise his consortium that for the long run without building and running a private server for
one dataset, which also runs against the open, reproducible spirit of the project. For now nothing changes:
AquaX stays in the v9 preview, behind the sign-in, because we have no replacement yet. The replacement is the
open OBIS models in the round 1 proposal I sent on September 16. I suggest we treat AquaX as a review-only
dataset and a benchmark, not publish a release that depends on it, and retire its files once the OBIS models
cover the same species.

Does that sound right to you? If so I'll let Gabriel know the plan, and we can take the internal server question to IT
together.

Cheers,
Ben
