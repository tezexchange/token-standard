contract type Callback = sig
  type storage
  val%entry main : bytes * address * address * nat * nat * nat -> _
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
   001 : owner's balance is not enough
   002 : receiver could not be the owner
*)

let%entry transfer 
          (receiver, amount, callback_option : address * nat * (Callback.instance * bytes) option) 
          storage =
  let owner_balance = 
    match Map.find (Current.sender ()) storage.balance_map with
    | None -> 0p
    | Some x -> x
  in
  match%nat owner_balance - amount with
  | Minus _ ->
    failwith ("Token", storage.symbol, "001")
  | Plus remain ->
    if Current.sender () = receiver then
      failwith ("Token", storage.symbol, "002")
    else
      let balance_map = Map.update (Current.sender ()) (Some remain) storage.balance_map in
      let receiver_balance = amount + (match Map.find receiver balance_map with | None -> 0p | Some x -> x) in
      let balance_map = Map.update receiver (Some receiver_balance) balance_map in
      let ops = 
        match callback_option with
        | None -> []
        | Some (callback_contract, passing_bytes) ->
          let arg = (passing_bytes, Current.sender (), receiver, amount, remain, receiver_balance) in
          [callback_contract.main arg 0tz]
      in
      ops, storage.balance_map <- balance_map


let%entry info (callback_contract : Callback.instance) storage =
  let passing = Bytes.pack (storage.symbol, storage.name, storage.decimal, storage.total) in
  let self_addr = Contract.address (Contract.self ()) in
  [callback_contract.main (passing, self_addr, Current.sender (), 0p, 0p, 0p) 0tz], storage


let%entry custom () _ =
  Current.failwith ()
