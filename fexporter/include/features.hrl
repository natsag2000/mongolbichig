%%  __  __  ___  _   _  ____  ___  _     ____ ___ ____ _   _ ___ ____
%% |  \/  |/ _ \| \ | |/ ___|/ _ \| |   | __ )_ _/ ___| | | |_ _/ ___|
%% | |\/| | | | |  \| | |  _| | | | |   |  _ \| | |   | |_| || | |  _
%% | |  | | |_| | |\  | |_| | |_| | |___| |_) | | |___|  _  || | |_| |
%% |_|  |_|\___/|_| \_|\____|\___/|_____|____/___\____|_| |_|___\____|
%%
-define(ampers, ampers).
-define(aphost, aphost).
-define(multiple, multiple).
-define(normal, normal).
-define(amperaphost, amperaphost).
-define(multipleaphost, multipleaphost).
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
