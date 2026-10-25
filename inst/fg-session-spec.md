# The FocusGroup session file

This note describes the session file of the R package FocusGroup, for a
person or a language model who turns the transcript of a focus group into
one. FocusGroup reads the file with `focus_group_from_transcript()` and
writes it with `format()` of a session result.

## Your task

You receive the transcript of a focus group: the words of a moderator and of
several participants, perhaps with notes. Write it out as a session file.

- Output the file and nothing else: no explanation, no code fences.
- Write one JSON object per line. Never spread an object over several lines,
  and never wrap the lines in an array.
- Copy every utterance exactly as written, with its fillers, errors, and
  bracketed remarks. Never paraphrase, translate, correct, shorten, or
  summarize.
- Never merge two turns into one or split one turn into two. A new turn
  begins only where the speaker changes or the transcript starts a new turn.
- Remove only speaker labels and timestamps from the start of a turn. Write a
  line break inside a turn as `\n`.
- Declare every speaker in a `participant` line before the first message, and
  use exactly the same id for that speaker in every message. Mark the
  moderator with `"is_moderator": true`, and only when the transcript names
  that person's role (moderator, facilitator, interviewer).
- Do not invent personas, ages, occupations, or anything else the transcript
  does not state. Leave out `persona` unless the transcript itself describes
  the speaker.
- A note that belongs to no speaker, such as "[break]", is left out. A remark
  inside someone's speech stays in that turn's text as written.

## Line types

Each line has a `type`. Write them in this order: `session` (at most one),
`participant` lines, then `message` lines in the order spoken. Files the
package writes also hold `item` lines after the participants and, after the
messages, `routing`, `probe`, `injection`, and `call` lines, the records of a
simulated session; leave those out when converting a transcript.

A field whose value is not known is left out. The fields below are the ones
a converted transcript needs; files the package writes carry more, the
columns of its result tables, described in `?format.focus_group_result`. A
field the format does not define is an error, which catches misspellings,
except that a field whose name starts with `x_` is ignored, so
`"x_source": "page 3"` can keep a note.

### `session`

Optional. Fields:

- `topic`: the subject of the discussion, in a few words.
- `purpose`: why the group met, if the transcript says.

### `participant`

One line per speaker, the moderator included. Fields:

- `id` (required): the speaker's label as the transcript gives it, such as
  `"Ana"` or `"Speaker 2"`. Never `"System"`.
- `is_moderator`: `true` for the moderator, otherwise left out.
- `persona`: a description of the speaker taken from the transcript, only if
  it gives one.

### `message`

One line per turn. Fields:

- `speaker_id` (required): the `id` of a declared speaker.
- `text` (required): the turn, copied exactly.
- `message_id`: a whole number, increasing down the file; leave it out on
  every line and the reader numbers the messages in order.
- `timestamp`: the time of the turn, if given, as ISO 8601 text
  (`"2026-03-01T14:30:05Z"`).

## What the reader does with a transcript

Each moderator turn becomes an item of the guide, as written. The handles the
agents would see are the speakers' ids. Problems are reported all at once,
each with its line number and field.

## Example 1

The transcript:

```
Moderator: Welcome, everyone. What do you make of the new library hours?
Ana: They help working parents like me; evenings matter most.
Ben: I see it differently. Mornings are now far too crowded.
Moderator: Ben, say more about what changed in the mornings.
Ben: Staff are stretched thin before noon, so, um, lines are longer.
```

The session file:

```
{"type":"session","topic":"library hours"}
{"type":"participant","id":"Moderator","is_moderator":true}
{"type":"participant","id":"Ana"}
{"type":"participant","id":"Ben"}
{"type":"message","speaker_id":"Moderator","text":"Welcome, everyone. What do you make of the new library hours?"}
{"type":"message","speaker_id":"Ana","text":"They help working parents like me; evenings matter most."}
{"type":"message","speaker_id":"Ben","text":"I see it differently. Mornings are now far too crowded."}
{"type":"message","speaker_id":"Moderator","text":"Ben, say more about what changed in the mornings."}
{"type":"message","speaker_id":"Ben","text":"Staff are stretched thin before noon, so, um, lines are longer."}
```

## Example 2

The transcript, with a heading, timestamps, and a speaker described by the
transcriber:

```
Clinic access group, 1 March 2026
[14:30:05] FACILITATOR (Rosa): Let's start. How do you get to the clinic?
[14:30:21] P1: By bus. [laughs] Two buses, actually.
[14:30:40] P2 (retired nurse): I drive, but parking is a problem.
[break]
[14:41:02] FACILITATOR (Rosa): Anything else before we close?
```

The session file:

```
{"type":"session","topic":"getting to the clinic"}
{"type":"participant","id":"Rosa","is_moderator":true}
{"type":"participant","id":"P1"}
{"type":"participant","id":"P2","persona":"retired nurse"}
{"type":"message","speaker_id":"Rosa","text":"Let's start. How do you get to the clinic?","timestamp":"2026-03-01T14:30:05Z"}
{"type":"message","speaker_id":"P1","text":"By bus. [laughs] Two buses, actually.","timestamp":"2026-03-01T14:30:21Z"}
{"type":"message","speaker_id":"P2","text":"I drive, but parking is a problem.","timestamp":"2026-03-01T14:30:40Z"}
{"type":"message","speaker_id":"Rosa","text":"Anything else before we close?","timestamp":"2026-03-01T14:41:02Z"}
```

The date in the timestamps comes from the heading; where the transcript gives
no date, leave `timestamp` out. The note "[break]" belongs to no speaker and is
left out; "[laughs]" is part of P1's turn and stays.
