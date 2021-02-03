# development

Run server for static files

    # NIXOS
    $> nix-shell -p pkgs.nodePackages.http-server --run "http-server --port 3001 --cors"
    
    # other
    $> npm install --global http-server
    $> http-server --port 3001 --cors

Run a mysql instance with database `podcast` present, and user `podcast`.

Serve homepage with default settings

    $> stack run -- --mysql-password password

Access page in browswer at `localhost:3000`.
