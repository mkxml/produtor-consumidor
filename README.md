## Problema do Produtor/Consumidor

Este repositório propõe uma solução para o problema do [produtor/consumidor](https://en.wikipedia.org/wiki/Producer–consumer_problem) codificada em [Erlang](http://erlang.org). A proposta do problema foi feita na disciplina de Sistemas Operacionais II na Universidade Feevale.

O problema em questão é um conhecido problema de concorrência quando desenvolvido paralelamente, onde existem três elementos chave:
  - **Buffer**
  - **Produtor**
  - **Consumidor**

O **Buffer** é a estrutura de memória compartilhada entre o produtor e o consumidor. O **produtor** adiciona elementos ao **buffer** e o **consumidor** remove elementos do **buffer**.

Se o **buffer** estiver cheio o produtor deve suspender suas atividades e esperar o consumidor liberar espaço. Além disso, se o **buffer** estiver vazio o **consumidor** deve suspender suas atividades e esperar o **produtor** carregar mais conteúdo.

O desafio acontece quando o produtor e o consumidor estão executando suas tarefas paralelamente, deve existir algum método para serializar as ações sobre o buffer. Caso contrário podem ocorrer dois problemas:
  1. Deadlocks: o **produtor** ou o **consumidor** pode ficar suspenso infinitamente caso o **buffer** seja modificado por outro processo/thread depois da validação de cheio ou vazio e antes da modificação do processo atual.

  2. Condições de corrida (racing condition): o **buffer** pode acabar com resultados indevidos devido a modificação durante a consulta de um processo pelo outro processo.

## Detalhes de implementação

Para resolver o problema implementamos o programa em três módulos executando em três processo diferentes e concorrentes:

  - Módulo `main` carregando o **Buffer** em formato de lista do Erlang;
  - Módulo `produtor` executando a função do **Produtor**;
  - Módulo `consumidor` executando a função do **Consumidor**.

Toda a implementação é baseada em duas características principais da linguagem funcional Erlang:
  1. Presença exclusiva de variáveis imutáveis;
  2. Caixa de mensagens serializada por processo.

### Processo main

O processo `main` é o inicial do programa inteiro, ele inicializa o **Buffer** e invoca os demais processos.

O sistema de comunicação com os outros processos é feito pelo sistema de troca de mensagens nativo do Erlang.

Tudo que acontece com o **Buffer** é controlado pelo `main`, serializando as ações em cima do **Buffer** garantindo a consistência do mesmo.

Ele recebe mensagens tanto do **produtor** quanto do **consumidor**, a propriedade de serialização das mensagens do Erlang garante que as ações serão aplicadas no **Buffer** na ordem que foram recebidas.

### Processo produtor

A tarefa do processo `produtor` é produzir valores randômicos simulando algum dado a ser posto no **Buffer**. Feito isso ele avisa o `main` que tem um novo conteúdo para o **buffer**, somente o `main` decide quando ele vai adicionar isso ao **buffer**.

### Processo consumidor

A tarefa do processo `consumidor` é consumir os dados gerados pelo `produtor` no **buffer**, reduzindo assim o conteúdo do mesmo.

Na prática existe um timer randômico que simula o consumo de um dado e devolve a instrução de remoção para o `main` executar no **buffer**.

## Observações da implementação

Programação funcional realmente é um conceito novo para nós, então levou algum tempo até pensar na lógica funcional do Erlang.

Além disso outros elementos como a falta de variáveis normais e pensamento recursivo, que são características de linguagens funcionais também exigiram uma adaptação.

Quanto ao Erlang em si criticamos a sintaxe verbosa demais dele assim como o operador de `=<` que é totalmente fora do padrão seguido por outras linguagens.

Pontos positivos do Erlang estão justamente relacionados com a programação paralela e distribuída entre múltiplos processadores e/ou computadores.

## Testando o programa

### Executando

Para executar o programa em sua máquina você precisa ter o Erlang/OTP instalado.

Feito isso basta seguir os passos:

  1. Clone o projeto com `git clone https://github.com/mkautzmann/produtor-consumidor.git`;
  2. Vá até o diretório do clone;
  3. Executa `erl -noshell -pa ebin -s main -s main start`;
  4. O programa agora estará rodando na sua CLI através da VM do Erlang.

### Debugando

O programa já foi compilado permitindo a utilização do debugger para verificar melhor o funcionamento do software.

Para executar o debugger você deve executar o programa de forma diferente. Siga os passos:

  1. Vá até o diretório do clone;
  2. Executa `erl -pa ebin`;
  3. Você estará na shell do Erlang, agora executa `debugger:start().`;
  4. A GUI do debugger irá aparecer. Selecione os módulos que deseja debugar e configurações que você quer setar e siga para o passo 5.
  5. Agora, dentro da shell do Erlang, execute `main:start().`;
  6. Pronto, você está debugando o programa.

### Fazendo o build

Caso você tenha feito alterações no código e deseja compilar recomendamos o [Rebar](), com ele instalado siga as instruções:

  1. Vá até o diretório raiz do seu clone;
  2. Execute `rebar compile`;
  3. Rode a aplicação normalmente.

### Contribuindo

## Autores

  - [Gustavo Wilrich](https://github.com/gustavo7w)
  - [Matheus Kautzmann](https://github.com/mkautzmann)

## Licença

  - [MIT License](LICENSE)
