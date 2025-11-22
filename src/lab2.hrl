-record(rbbag,
        {color :: red | black, data :: {integer(), any()}, left :: rbbag(), right :: rbbag()}).

-type rbbag() :: nil | #rbbag{}.
