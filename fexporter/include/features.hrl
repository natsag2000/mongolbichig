-record(feature,
        {
          name="",
          lookups=[]
         }).
-record(lookup,
        {
          name="",
          lookupflag="",
          lookups=[]
         }).

-record(lookuptable,
        {
          sub=[],
          by=[]
         }).

-define(ampers, ampers).
-define(multiple, multiple).
-define(normal, normal).
-define(aphost, aphost).
-define(amperaphost, amperaphost).
-define(multipleaphost, multipleaphost).
