let inputs : Scope.inputs =
  { columns = (fun ~missing path -> Data_file.columns ~missing path)
  ; digest = Data_file.digest
  ; contract = Data_dict.import
  }
;;
