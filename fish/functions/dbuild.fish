function dbuild
    echo "building $argv"
    docker build -t $argv .
end
