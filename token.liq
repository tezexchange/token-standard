contract type Callback = sig
  type storage
  val%entry main : bytes -> _
end

type storage = {
  balance_map : (address, nat) big_map;
  symbol : string;
  name : string;
  decimal : nat;
  total : nat;
}

let%init storage = {
  balance_map = (BigMap [(tz1AAAAAAAAAAAAAAAAAAAAAAAAAAAAowner : address), 10000p;] : (address, nat) big_map);
  symbol = "SAMPLE";
  name = "sample token";
  decimal = 2p;
  total = 10000p;
}

(* error list 
   100 : owner's balance is not enough
*)

let%entry info (callback_contract : Callback.instance) storage =
  let arg = Bytes.pack (storage.symbol, storage.name, storage.decimal, storage.total) in
  [callback_contract.main arg 0tz], storage


let%entry transfer 
          (receiver, amount, callback_option : address * nat * (Callback.instance * bytes) option) 
          storage =
  let[@inline] owner = Current.sender () in
  let owner_balance = 
    match Map.find owner storage.balance_map with
    | None -> 0p
    | Some x -> x
  in
  let storage, owner_balance, receiver_balance = 
    match%nat owner_balance - amount with
    | Minus _ ->
      Current.failwith (String.concat ["TOKEN/"; storage.symbol; "/100"])
    | Plus remain ->
      if owner = receiver then
        storage, owner_balance, owner_balance
      else
        let balance_map = Map.update owner (Some remain) storage.balance_map in
        let receiver_balance = amount + (match Map.find receiver balance_map with | None -> 0p | Some x -> x) in
        let balance_map = Map.update receiver (Some receiver_balance) balance_map in
        (storage.balance_map <- balance_map), remain, receiver_balance
  in
  let ops = 
    match callback_option with
    | None -> ([] : operation list)
    | Some (callback_contract, passing_bytes) ->
      let arg = Bytes.pack (passing_bytes, owner, receiver, amount, owner_balance, receiver_balance) in
      [callback_contract.main arg 0tz]
  in
  ops, storage


let%entry custom () _ =
  Current.failwith ()
