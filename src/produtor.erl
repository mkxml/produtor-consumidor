%% Arquivo do Produtor

%% Este arquivo representa o produtor, que adiciona conteúdo no buffer.

-module(produtor).
-mode(compile).
-author("Gustavo Wilrich").
-author("Matheus Kautzmann").
-export([loop/2]).

% Produz um número randômico de 1 a 9
produzir() ->
  random:seed(now()),
  random:uniform(9).

loop(Mestre, Timer) ->
  receive
    {Mestre, cheio} ->
      %% Já enchi o buffer, avisar o mestre que dormi para efeitos de LOG
      Mestre ! {self(), dormi},
      loop(Mestre, Timer);
    {Mestre, _} ->
      %% Seta um timer randômico de até Timer segundos para simular a produção de conteúdo
      random:seed(now()),
      timer:sleep(round(timer:seconds(random:uniform(Timer)))),
      Valor = produzir(),
      Mestre ! {self(), adiciona, Valor},
      loop(Mestre, Timer);
    _ ->
      Mestre ! {erro, "Mensagem não reconhecida"},
      loop(Mestre, Timer)
  end.
