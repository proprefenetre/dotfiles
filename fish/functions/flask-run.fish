function flask-run
    set -lx FLASK_ENV "development"
    set -lx FLASK_APP $argv
    flask run
end
