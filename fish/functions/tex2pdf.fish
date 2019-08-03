function tex2pdf
    xelatex $argv
    and open (string split -r -m1 . $argv)[1]".pdf"
end
