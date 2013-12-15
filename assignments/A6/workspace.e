class WORKSPACE
creation make
feature {ANY}
    space : ARRAY[STRING]
    curi : INTEGER
    make is
        do
            create space.make(0,0)
            curi := 0
        end
    addtospace (ele:STRING) is
        do
            space.add_last(ele)
        end
    spacedone:BOOLEAN is
        do
            Result := curi = space.count
        end
    setcuridone is
        do
            curi := space.count;
        end
    dump is
        local 
            i : INTEGER
        do
            from
                i := 1 
            until
                i = space.count
            loop
                if (space@i) = void then
                else
                        std_output.put_string(" "+ (space@i))
                end
                i := i+1
            end
        end
end
