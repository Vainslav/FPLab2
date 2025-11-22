-record(rbbag, {
    color :: red | black,
    data :: {integer(), any()},
    left :: nil | rbbag,
    right :: nil | rbbag
}).

-type rbbag() :: nil | #rbbag{}.