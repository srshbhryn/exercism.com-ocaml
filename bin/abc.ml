let hello who = "
  <html>
  <body>
    <h1>Hello, "  ^ who ^  "!</h1>
  </body>
  </html>
"

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/" (fun _ ->
      Dream.html (hello "world"));
  ]
