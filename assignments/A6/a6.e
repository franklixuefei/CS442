class A6 
creation make
feature {ANY}
    make is
        local 
            lookup : DICTIONARY[NODE, STRING]
            tempno : NODE

            workspaces : ARRAY[WORKSPACE]
            tempspace : WORKSPACE

            tok : TOKENIZER
            s : STRING
            currenthead : STRING
            inrule : BOOLEAN

            argc : INTEGER
            i : INTEGER

            copycounter :INTEGER
        do
            inrule := False
            create lookup.make;
            create tempno.make;
            argc := argument_count;
            i := 0;
            copycounter := 0;
            create workspaces.make(0,0)
--            lookup.put(tempno, "hi");
--            (lookup@"hi").insertrule("b");
--            (lookup@"hi").insertrule("b");
--            (lookup@"hi").insertrule("b");

            !!tok.make(argument(1))
            from
                s := ""
            until
                s.is_equal("EOF")
            loop
                s := tok.next_token
                if s.is_equal(".") then
                    (lookup@currenthead).closerule
                    inrule := False
                elseif s.is_equal(":-") then
                    inrule := True
                else
                    if inrule then
                        (lookup@currenthead).insertrule(s)
                    else
                        currenthead := s
                        if lookup.has(s) then
                        else
                            create tempno.make
                            lookup.put(tempno, s);
                        end
                    end
                end
           end
-- setup the first rule
           create tempspace.make;
--           workspaces.add_last(tempspace);
           from
               i := 1
           until
               i = argc
           loop
               i := i+1;
               tempspace.addtospace(argument(i));
           end
            
          create tempspace.make

          from

          until
              workspaces.count = 0
          loop
             create tempspace.make
             from
                 copycounter :=0;
             until
                 copycounter >= workspaces.last.space.count
             loop
                 tempspace.addtospace(workspaces.last.item(copycounter))
                 copycounter := copycounter +1;
             end
--              if 
--              (workspaces@1).setcuridone;
          end
            --std_output.put_string("My name is: " + (lookup@"hi"))
            workspaces.last.dump;
            std_output.put_new_line 
        end
dumptable is
    do

    end
end -- class A6
