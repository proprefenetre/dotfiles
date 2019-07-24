function docker-compose-alt
    docker run -it --rm --name=compose -v /var/run/docker.sock:/var/run/docker.sock docker/compose:1.24.1
end
