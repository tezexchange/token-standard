contract type Callback = sig
  type storage
  val%entry main : bytes -> storage -> operation list * storage
end

contract Token = struct
  type storage = {
    balance_map : (address, nat) big_map;
    symbol : string;
    name : string;
    decimal : nat;
    total : nat;
  }

  (* error list 
     100 : owner's balance is not enough
  *)

  let%entry info (callback_contract : Callback.instance) storage =
    [Contract.call callback_contract 0tz (Bytes.pack (storage.symbol, storage.name, storage.decimal, storage.total))], storage


  let%entry transfer (params : address * nat * (Callback.instance * bytes) option) storage =
    let receiver, amount, callback_option = params in
    let owner = Current.sender () in
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
        [Contract.call callback_contract 0tz (Bytes.pack (passing_bytes, receiver, amount, owner_balance, receiver_balance))]
    in
    ops, storage


  let%entry custom (p : nat * bytes) s =
    Current.failwith (p, s)
end