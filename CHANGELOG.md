0.3.0.0 (December 2, 2018)
==========================
 
- Added support for specifying a Status Callback URL when POST-ing to the
  Messages endpoint. (#64)

0.2.0.1 (May 14, 2018)
======================

- Support template-haskell >=2.12, relax upper bound on free (#61)

0.2.0.0 (January 29, 2018)
==========================

This release mainly refactors the way SIDs are handled internally. Previously,
adding a SID required copying a bunch of boilerplate. Now we use Template
Haskell to generate that boilerplate automatically, and adding a SID is as easy
as calling `createSID`. Each SID is a newtype wrapper around a base data type,
`SID`. So, for example, `AccountSID` now wraps a `SID A C`.

Bug Fixes
---------

- `MessageSID` only handled "SM"-prefixed SIDs; however, there are "MM"-prefixed
  Message SIDs, too. So, now we have two data types, `SMSMessageSID` and
  `MMSMessageSID` corresponding to the "SM"- and "MM"-prefixed variants, and
  `MessageSID` becomes a newtype wrapper around
  `Either SMSMessageSID MMSMessageSID`.

Features
----------

- Add support for deleting recordings.
- Add [Messaging Copilot](https://www.twilio.com/copilot) support via a new
  function, `postCopilot`.
