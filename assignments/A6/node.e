class NODE
creation make
feature {}
    ruletable : ARRAY[ARRAY[STRING]]
    ruleopen : BOOLEAN
feature
    make is 
do
    create ruletable.make(0,0)
    ruleopen := false
end

insertrule(val:STRING) is
        local
            tempa : ARRAY[STRING]
        do
            if ruleopen then
                ruletable.last.add_last(val)
            else
                ruleopen := True
                create tempa.make(0, 0)
                tempa.add_last(val)
                ruletable.add_last(tempa)
            end
        end
closerule is
    do
        ruleopen := false
    end

dump is
    local
        i:INTEGER
        j:INTEGER
    do 
        from 
            i:=0
        until
            i >= ruletable.count
        loop
            if (ruletable@i) = void then
            else
                std_output.put_string("Entry " + i.to_string + " [")
                from
                    j:=0
                until
                    j>= (ruletable@i).count
                loop
                    if ((ruletable@i)@j) = void then
                    else
                        std_output.put_string(" "+ ((ruletable@i)@j))
                    end
                    j := j+1
                end
                std_output.put_string("]")
                std_output.put_new_line 
            end
            i:= i+1
        end
end
end

