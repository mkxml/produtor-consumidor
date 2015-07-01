%% Arquivo do Consumidor

%% Este arquivo representa o consumidor, que consume conteúdo no buffer.

-module(consumidor).
-mode(compile).
-author("Gustavo Wilrich").
-author("Matheus Kautzmann").
-export([loop/2]).

loop(Mestre, Timer) ->
  receive
    {Mestre, vazio} ->
      %% Já esvaziei o buffer, avisar o mestre que dormi para efeitos de LOG
      Mestre ! {self(), dormi},
      loop(Mestre, Timer);
    {Mestre, _} ->
      %% Seta um timer randômico de até Timer segundos para simular o consumo de conteúdo
      random:seed(now()),
      timer:sleep(round(timer:seconds(random:uniform(Timer)))),
      Mestre ! {self(), remove},
      loop(Mestre, Timer);
    _ ->
      Mestre ! {erro, "Mensagem não reconhecida"},
      loop(Mestre, Timer)
  end.
