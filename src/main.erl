%% Arquivo principal do problema do Produtor/Consumidor

%% Este arquivo representa o processo mestre, que possui o buffer.

%% Importante, o tamanho completo do buffer está definido na macro TAMANHO.

%% OBS: Primeiro programa Erlang da vida, então devem existir formas melhores de resolver o problema.

-module(main).
-mode(compile).
-author("Gustavo Wilrich").
-author("Matheus Kautzmann").
-behaviour(application).

%% Funções de início e parada e controla_buffer
-export([start/0, start/2, stop/1, loop_controla_buffer/3]).

%% Define o tamanho do buffer em 10
-define(TAMANHO, 10).

%% Define o timer máximo para o timer randômico do produtor em SEGUNDOS
-define(MAX_TIMER_PRODUTOR, 5).

%% Define o timer máximo para o timer randômico do consumidor em SEGUNDOS
-define(MAX_TIMER_CONSUMIDOR, 5).

%% Se o buffer estava vazio, adiciona o valor e retorna também o átomo estava_vazio
adiciona_ao_buffer([], Valor) ->
  {estava_vazio, [Valor]};
adiciona_ao_buffer(Buffer, Valor) when length(Buffer) < ?TAMANHO ->
  {ok, Buffer ++ [Valor]};
%% Se não o buffer está cheio retorna o Buffer atual e o átomo cheio
adiciona_ao_buffer(Buffer, _) ->
  {cheio, Buffer}.

remove_do_buffer(Buffer) when length(Buffer) =:= ?TAMANHO ->
  %% Remove a cabeça
  [_|T] = Buffer,
  {estava_cheio, T};
remove_do_buffer(Buffer) when length(Buffer) > 0 ->
  % Devolve somente a cauda, removendo a cabeça
  [_|T] = Buffer,
  {ok, T};
%% Se não há nada no buffer, retorna ele e o átomo vazio
remove_do_buffer(Buffer) ->
  {vazio, Buffer}.

%% ===================================================================
%% Função que escuta os eventos do Buffer
%% ===================================================================

loop_controla_buffer(Produtor, Consumidor, Buffer) ->
  receive
    %% Recebe mensagens apenas do PRODUTOR
    {Produtor, adiciona, Valor} ->
      io:format("Produziu ~p~n", [Valor]),
      %% Executa a adição no buffer, criando um novo buffer
      {Status, NovoBuffer} = adiciona_ao_buffer(Buffer, Valor),
      %% Se estava vazio acorda Consumidor informando novos dados
      if
        Status =:= estava_vazio ->
          io:format("Acordando consumidor...~n"),
          Consumidor ! {self(), acorda};
        Status =:= cheio ->
          io:format("Buffer cheio, descartando produção!~n");
        true ->
          ok
      end,
      io:format("Buffer atual: "),
      io:write(NovoBuffer),
      io:format("~n"),
      %% Entrega um OK ou CHEIO para o produtor
      Produtor ! {self(), Status},
      loop_controla_buffer(Produtor, Consumidor, NovoBuffer);

    %% Produtor sinalizando que dormiu
    {Produtor, dormi} ->
      io:format("Produtor dormindo...~n"),
      loop_controla_buffer(Produtor, Consumidor, Buffer);

    %% Recebe mensagens apenas do CONSUMIDOR
    {Consumidor, remove} ->
      io:format("Consumindo...~n"),
      %% Executa a remoção no buffer, criando um novo buffer
      {Status, NovoBuffer} = remove_do_buffer(Buffer),
      %% Se estava cheio acorda Produtor informando novos dados.
      if
        Status =:= estava_cheio ->
          io:format("Acordando produtor...~n"),
          Produtor ! {self(), acorda};
        Status =:= vazio ->
          io:format("Buffer vazio, parando consumo!~n");
        true ->
          ok
      end,
      io:format("Buffer atual: "),
      io:write(NovoBuffer),
      io:format("~n"),
      %% Entrega um OK ou VAZIO com o buffer novo para consumidor
      Consumidor ! {self(), Status},
      loop_controla_buffer(Produtor, Consumidor, NovoBuffer);

    % Consumidor sinalizando que dormiu
    {Consumidor, dormi} ->
      io:format("Consumidor dormindo...~n"),
      loop_controla_buffer(Produtor, Consumidor, Buffer);

    %% Mensagens fantasma aparecerão no log
    _ ->
      io:format("Mensagem não reconhecida recebida!~n"),
      loop_controla_buffer(Produtor, Consumidor, Buffer)
  end.

%% ===================================================================
%% Início
%% ===================================================================

start() ->
  start([],[]).

start(_StartType, _StartArgs) ->
  io:format("~ts~n", ["Iniciando o programa Produtor/Consumidor"]),
  io:format("Setando o buffer incial vazio~n"),
  io:format("Criando Produtor e Consumidor~n"),
  Produtor = spawn(produtor, loop, [self(), ?MAX_TIMER_PRODUTOR]),
  Consumidor = spawn(consumidor, loop, [self(), ?MAX_TIMER_CONSUMIDOR]),
  BufferInicial = [],
  io:format("~ts~n~n", ["Buffer esperando..."]),
  %% Inicia todo o ciclo!
  Produtor ! {self(), acorda},
  %% Aqui a mágica acontece, a função a seguir escuta os eventos e controla todo o buffer
  loop_controla_buffer(Produtor, Consumidor, BufferInicial).

stop(_State) ->
    ok.
