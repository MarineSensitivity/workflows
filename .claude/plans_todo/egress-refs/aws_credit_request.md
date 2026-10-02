# AWS billing adjustment request (draft, 2026-10-02)

## Where to send it

AWS Support Center, a billing case. Billing cases are free on every support plan (the Support API is not, so
this cannot be filed from the CLI on a Basic plan).

1. Sign in as the account owner: <https://support.console.aws.amazon.com/support/home#/case/create>
2. Choose **Account and billing**.
3. Service: **Billing**. Category: **Charge Inquiry** (or **Dispute a Charge**, whichever the form offers).
4. Severity: **General question**. Contact method: Web (they reply in the case and by email).
5. Paste the subject and description below. Attach a screenshot of the Cost Explorer daily chart for
   `DataTransfer-Out-Bytes` (Cost Explorer → Group by: Usage type → filter Service = EC2-Instances, daily,
   2026-08-20 to today). The flat step on Aug 27 makes the case by itself.

Send it **after** everything under "What I have done" is true, because the reviewer will ask what was fixed
and what prevents a repeat: the Caddy deploy has stopped the transfer (a full day of CloudWatch `NetworkOut`
under 2 GB), the mirror's key is disabled, and the plan's P1 guardrails exist (the NetworkOut alarms, the cost
anomaly monitor, the two budgets). Until P1 is applied, items 2 and 3 below are not yet true: delete them or
wait. A credit is at AWS's discretion; a first-time, clearly accidental, already-fixed charge is the kind they
usually consider. Update the October figure to the day the transfer stopped.

## Subject

Request for a one-time billing adjustment: unintended EC2 data transfer out, 2026-08-27 to 2026-10-02

## Description

Account: 814665782451. Region: us-east-1. Instance: i-0692d15b330da30b6 (msens1, t2.xlarge).

I am asking for a one-time credit for unintended data transfer charges caused by a misbehaving automated
client, which I have now stopped.

What was charged (usage type `DataTransfer-Out-Bytes`, service EC2-Instances):

- August 2026: 702.8 GB, $55.60. Usage was 0.1 to 0.2 GB per day until August 26, then 55.8 GB on August 27
  and about 153 GB per day from August 28.
- September 2026: 4,518.9 GB, $397.70. About 153 GB every day.
- October 1 to 2, 2026: about 280 GB, roughly $25 (until the fix below).

Before August 27 this account's data transfer out stayed inside the free allowance.

What caused it: a partner organization's server mirrors files from this instance over SFTP once an hour. On
2026-08-27 at 17:05 UTC that server's disk became full. From then on, every hour, its sync tool requested the
same 3,950 files (3.2 GB), failed to write them, retried, and repeated the next hour. CloudWatch `NetworkOut`
for the instance shows one burst of about 5.8 GB in a single five-minute period each hour and almost nothing
in between. No legitimate user traffic was involved; the instance's web server sent about 1 GB per day in total
over the same period.

What I have done:

1. Removed the files from the directory the mirror reads, so its hourly sync has nothing to transfer, and I am
   retiring that mirror's access altogether.
2. Created CloudWatch alarms on the instance's `NetworkOut` (hourly and daily thresholds) and an AWS Cost
   Anomaly Detection monitor with email alerts.
3. Replaced an unrealistic account budget with one at the real baseline plus a separate budget on data
   transfer.

Request: a one-time credit for the EC2 `DataTransfer-Out-Bytes` charges for August 27, 2026 through October 2,
2026 (about $475: $55.60 in August, $397.70 in September, and October to date). I can provide the CloudWatch
series, the Cost Explorer export, or the client's own log showing the failed retries.

Thank you.
