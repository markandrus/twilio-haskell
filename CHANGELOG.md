0.2.0.0 (in progess)
====================

This release mainly refactors the way SIDs are handled internally. Previously,
adding a SID required copying a bunch of boilerplate. Now we use Template
Haskell to generate that boilerplate automatically, and adding a SID is as easy
as calling `createSID`. Each SID is a newtype wrapper around a base data type,
`SID`. So, for example, `AccountSID` now wraps a `SID A C`.

Bug Fixes
---------

- 'MessageSID' only handled "SM"-prefixed SIDs (#48); however, there are
  "MM"-prefixed Message SIDs, too. So, now we have two data types,
  `SMSMessageSID` and `MMSMessageSID` corresponding to the "SM"- and
  "MM"-prefixed variants, and `MessageSID` becomes a newtype wrapper around
  `Either SMSMessageSID MMSMessageSID`.
