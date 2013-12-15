class TOKENIZERCLIENT
  creation make
  feature
    tok : TOKENIZER
    make is
      local s : STRING
      do
        !!tok.make(argument(1))
        from
          s := ""
        until
          s.is_equal("EOF")
        loop
          s := tok.next_token
          io.put_string(s)
          io.put_new_line
        end
      end
end -- class TOKENIZERCLIENT
