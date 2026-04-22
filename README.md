# Telemetry Vision System

# 1. Visão Geral do Projeto
O Telemetry Vision System é uma aplicação web em R construída com Rhino/Shiny para operar um sistema de visão voltado a cadastro, organização, análise e treinamento de dados de câmeras, setores, objetos, estruturas, alarmes e pacotes de treino. O projeto não é apenas um “front-end de cadastro”; ele organiza um modelo operacional completo para relacionar fontes de vídeo, estruturas de objetos, contexto temporal e geração de artefatos de treino.

Em termos práticos, o sistema resolve o problema de centralizar em uma única interface:
- o cadastro de câmeras e seus parâmetros operacionais;
- o cadastro de setores com janelas de reativação e histórico;
- o cadastro de objetos e suas relações com câmeras, setores, estrutura e geometria;
- a definição de estruturas e atributos que descrevem o comportamento observado;
- a criação de regras de alarme baseadas em expressões sobre atributos;
- a preparação de pacotes de treino com frames e contexto;
- a navegação analítica sobre o fluxo temporal dos eventos.

O contexto de uso é de operação industrial/telemetria visual, onde câmeras observam áreas, objetos são modelados com contexto e o sistema precisa transformar vídeo em informação estruturada. A proposta operacional do projeto é permitir que a equipe técnica configure a base sem acessar diretamente o banco ou escrever scripts manuais a cada alteração.

A visão macro do sistema é esta:
- o usuário entra;
- informa e valida a conexão com o banco;
- navega por menus laterais modais;
- cria ou edita entidades do domínio;
- grava tudo no banco relacional;
- usa os dados para consulta, rastreamento, alarmes e geração de treino.

# 2. Objetivo do Sistema
A finalidade central do sistema é estruturar um pipeline funcional de visão computacional aplicada à operação. Isso inclui cadastro, relacionamento, consulta temporal, monitoramento e geração de datasets para treino.

O valor funcional que ele entrega está em três frentes:
- organização operacional: cada câmera, setor, objeto e estrutura passa a existir com identidade e vínculo claros;
- rastreabilidade: o sistema permite voltar do evento observado para o contexto, para a geometria e para o período temporal;
- preparação para modelagem: os pacotes de treino e a exportação em `.rds` permitem uso posterior em análise e aprendizado de máquina.

Os cenários principais de uso são:
- cadastrar câmeras com URL e FPS;
- cadastrar setores com regras temporais;
- cadastrar objetos vinculados a setores e câmeras;
- configurar estruturas e atributos para caracterizar a observação;
- configurar alarmes a partir de expressões sobre atributos;
- montar pacotes de treino com frames e contexto;
- compor saídas de treino em arquivo;
- consultar contexto histórico e navegar visualmente pelos fluxos.

# 3. Arquitetura Geral
A arquitetura é modular e segue uma divisão clara entre interface, regras de negócio, infraestrutura de banco e processamento assíncrono.

Em alto nível, o sistema se divide em:
- camada de apresentação, em `app/view/`, responsável por modais, tabelas, dashboards e navegação;
- camada de lógica/acesso a dados, em `app/logic/`, responsável por consultas, validações, montagem de payloads e regras de seleção;
- camada de infraestrutura, em `app/infra/`, responsável por pool de conexão, transações e helpers de banco;
- camada de modelo/reuso de interface, em `app/model/`, com componentes como `Swiper` e modais de confirmação;
- camada de assets e comportamento cliente, em `app/js/` e `app/styles/`.

O fluxo real de dados percorre o sistema assim:
- a UI abre um modal de operação;
- os inputs são capturados no cliente e normalizados no servidor;
- a lógica valida unicidade, presença obrigatória, consistência temporal e relacionamento entre entidades;
- a persistência grava em PostgreSQL via pool;
- a UI atualiza listas, mensagens, gráficos ou navegação de slide.

A interface não conversa diretamente com SQL bruto espalhado por toda parte. Em vez disso, cada menu aciona funções específicas de DAO e helpers centralizados. Isso reduz acoplamento e faz o comportamento se concentrar no módulo correto.

Há também um padrão importante de navegação:
- o sidebar seleciona um submenu;
- o sistema abre uma tela modal dedicada;
- ao concluir, o submenu volta para `noop`;
- o foco retorna à área principal sem deixar a seleção lateral “presa”.

Essa decisão arquitetural sustenta a usabilidade e permite que os módulos funcionem como fluxos guiados e não como páginas soltas.

Arquivos-chave para entender essa arquitetura:
- [app/main.R](C:/Sistema/GitHub/telemetry-vision-system/app/main.R)
- [app/view/init.R](C:/Sistema/GitHub/telemetry-vision-system/app/view/init.R)
- [app/infra/db_pool.R](C:/Sistema/GitHub/telemetry-vision-system/app/infra/db_pool.R)
- [app/infra/database.R](C:/Sistema/GitHub/telemetry-vision-system/app/infra/database.R)
- [document/script.sql](C:/Sistema/GitHub/telemetry-vision-system/document/script.sql)

# 4. Estrutura do Projeto
A organização do repositório sustenta o sistema por responsabilidades bem separadas.

- `app.R`: runner do processo; hoje chama `shiny::runApp(app, host = "127.0.0.1", port = 5000)`. Há um bloco comentado com boot de `mirai`, mas ele não está ativo.
- `app/main.R`: ponto de entrada Rhino para exportar `ui` e `server`.
- `app/view/`: camada principal de interface. Contém login, layout, menus, modais, dashboards, gráficos, tema, componentes e helpers visuais.
- `app/logic/`: DAOs e regras de acesso/transformação. Aqui ficam as consultas de câmera, setor, objeto, estrutura, alarme, dashboard, chat, treino e utilitários de frames.
- `app/infra/`: pool de conexão e helpers de execução/transação.
- `app/model/`: componentes reutilizáveis como `Swiper` e caixa de confirmação.
- `app/js/`: scripts client-side de apoio.
- `app/styles/`: estilos próprios do sistema.
- `app/static/`: assets estáticos, sons, favicon e bundles exportados.
- `document/`: scripts SQL e documentação estrutural do banco.
- `tests/`: testes de integração e e2e.
- `train/`: artefatos `.rds` de treino e conjuntos derivados.
- `tmp/`: arquivo de trabalho/variante de dashboard.
- `renv/` e `renv.lock`: ambiente reprodutível de dependências.

Arquivos de referência estrutural:
- [app/view/init.R](C:/Sistema/GitHub/telemetry-vision-system/app/view/init.R)
- [app/view/global.R](C:/Sistema/GitHub/telemetry-vision-system/app/view/global.R)
- [app/view/model.R](C:/Sistema/GitHub/telemetry-vision-system/app/view/model.R)
- [document/struct.sql](C:/Sistema/GitHub/telemetry-vision-system/document/struct.sql)
- [tests/cypress/e2e/app.cy.js](C:/Sistema/GitHub/telemetry-vision-system/tests/cypress/e2e/app.cy.js)

Observação estrutural importante:
- `document/script.sql` representa a modelagem mais ampla e atualizada do banco.
- `document/struct.sql` preserva uma versão anterior/mais enxuta do esquema, com foco em câmera, objeto e estrutura.
- O código principal do app está mais alinhado com o modelo expandido de `script.sql`.

# 5. Mapa Completo dos Menus

## Camera
- Objetivo do menu: cadastrar e manter câmeras de captura.
- Problema que resolve: centraliza a definição da fonte de vídeo, o FPS de processamento e a ativação operacional da câmera.
- Contexto de uso: usado quando uma nova fonte RTSP ou similar precisa ser adicionada, ou quando uma câmera existente precisa ser editada.
- Funcionalidades disponíveis: criação de câmera, listagem, edição, remoção e atualização de estado.
- Ações do usuário: inserir nome, URL, FPS, ativar/desativar, salvar, abrir a lista e editar registros.
- Dados de entrada: nome da câmera, URL, FPS, flag de ativo.
- Dados processados: o nome é normalizado em maiúsculas; a URL é validada; o FPS é persistido em `camera_config`; a existência de nome/URL duplicados é checada.
- Dados de saída: registro em `camera_view` e `camera_config`, lista atualizada e feedback visual.
- Componentes acionados: `uiNewCamera`, `uiEditCamera`, `selectAllCameras`, `selectAllCamerasWithFrameStatus`, `insertNewCamera`, `updateCamera`, `deleteCamera`, `checkifExistNameCamera`, `checkifExistUrlCamera`.
- Lógica interna: a tela de edição é multi-slide com `Swiper`; a listagem abre a edição do item escolhido; o salvamento roda em transação.
- Validações ou regras aplicadas: nome obrigatório; URL obrigatória; nome e URL não podem duplicar; câmera recebe `fg_ativo`; o `fps_camera` é salvo junto da configuração.
- Dependências: PostgreSQL via pool, `DT`, `Swiper`, helpers de modal/alerta, `actionWebUser`.
- Relação com outros menus: alimenta `Objeto`, `Treinar`, `Dashboard`, `Flow Graphic` e qualquer fluxo que dependa de frames por câmera.
- Impacto no fluxo geral: é uma entidade base; sem câmera não existe rastreamento visual nem geração de contexto temporal.
- Justificativa arquitetural: separa a origem do vídeo da lógica de análise e mantém o sistema capaz de rastrear múltiplas câmeras.

## Setor
- Objetivo do menu: cadastrar setores operacionais e seus parâmetros temporais.
- Problema que resolve: organiza o contexto físico/operacional onde objetos e câmeras se relacionam.
- Contexto de uso: usado para criar áreas funcionais, linhas, células ou regiões de produção.
- Funcionalidades disponíveis: criação, listagem, edição e exclusão.
- Ações do usuário: informar nome, tempo de reativação, unidade de reativação, tempo de passado, unidade de passado, salvar e editar.
- Dados de entrada: nome do setor e dois pares tempo/unidade.
- Dados processados: o nome é normalizado; os tempos são persistidos em colunas específicas; a listagem mostra quantidade de objetos vinculados.
- Dados de saída: registros em `setor`, lista com contagem de objetos associados e confirmação de gravação.
- Componentes acionados: `uiNewSetor`, `uiEditSetor`, `uiMain`, `selectAllSetors`, `insertNewSetor`, `updateSetor`, `deleteSetor`, `checkifExistNameSetor`.
- Lógica interna: a edição também é multi-slide com `Swiper`; o menu lista setores com ações por linha; as alterações são transacionais.
- Validações ou regras aplicadas: nome obrigatório; nome não pode duplicar; os tempos são obrigatórios; unidades aceitas incluem `Minuto` e `Hora`.
- Dependências: pool de banco, DAOs, `Swiper`, `DT`, `actionWebUser`.
- Relação com outros menus: é base para `Camera`, `Objeto`, `Alarme`, `Treinar` e relatórios por setor.
- Impacto no fluxo geral: define escopo operacional e janelas temporais usadas por fluxos analíticos.
- Justificativa arquitetural: o setor é o contêiner lógico que dá sentido territorial e temporal ao restante do domínio.

## Objeto
- Objetivo do menu: cadastrar objetos observados pelo sistema e administrar seu contexto, geometria e vínculo com câmeras e estruturas.
- Problema que resolve: representa a entidade que será rastreada, interpretada ou usada em geração de contexto.
- Contexto de uso: usado quando uma máquina, item, área ou elemento visual precisa ser modelado.
- Funcionalidades disponíveis: criação, listagem, edição, contexto histórico e edição de geometria/estrutura.
- Ações do usuário: escolher setor, nome, ativo/inativo, desenvolvimento, agrupamento, ID de grupo, tipo do objeto, câmeras associadas e configuração geométrica.
- Dados de entrada: setor, nome, flags, tipo do objeto, múltiplas câmeras, estrutura e desenhos no mapa/leaflet.
- Dados processados: o objeto pode ser tratado como estático ou dinâmico; componentes recebem cores; polígonos e retângulos são persistidos; o contexto histórico é consultado por intervalo.
- Dados de saída: registros em `objeto`, `objeto_config`, `componente`, `objeto_contexto` e, no caso de contexto, tabelas e visualizações temporais.
- Componentes acionados: `uiNewObjeto`, `uiEditObjeto`, `uiObjetoContexto`, `selectAllObjetos`, `selectObjetoById`, `selectObjetoContexto`, `insertNewObjeto`, `insertNewObjetoConfig`, `insertNewComponente`, `insertNewAtributo`, `updateObjeto`, `deleteObjeto`, `deleteObjetoContextoByPeriodo`.
- Lógica interna: o menu usa múltiplos slides e diversas interações com `leaflet`; em objetos dinâmicos, o código habilita seleção e rastreamento de retângulos; em objetos estáticos, o foco é desenho e relacionamento com componentes/estrutura.
- Validações ou regras aplicadas: nome obrigatório; o objeto não pode duplicar; a lista de câmeras precisa existir; o fluxo de contexto exige intervalo temporal válido; o tipo do objeto muda o comportamento da UI.
- Dependências: câmeras válidas, setores válidos, estruturas existentes, `leaflet`, `leaflet.extras`, `DT`, `Swiper`, helpers de comparação e alertas.
- Relação com outros menus: é o ponto de convergência de `Camera`, `Setor`, `Estrutura`, `Treinar` e `Alarme`.
- Impacto no fluxo geral: é a entidade mais importante do domínio, porque concentra a observação operacional e serve de base para contexto, alarme e treino.
- Justificativa arquitetural: o sistema precisa de um objeto central para ligar vídeo, geometria, contexto histórico e aprendizado.

## Estrutura
- Objetivo do menu: definir a estrutura conceitual associada ao objeto e seus atributos.
- Problema que resolve: permite modelar quais propriedades são relevantes para interpretação do objeto.
- Contexto de uso: usado quando é preciso descrever uma estrutura, seus atributos e o tipo de dado de cada atributo.
- Funcionalidades disponíveis: cadastro, listagem, edição, remoção de atributos, seleção de tipos de dados, ativação de atributos.
- Ações do usuário: nomear estrutura, adicionar/remover atributos, marcar ativo, selecionar tipo de dado, salvar e atualizar.
- Dados de entrada: nome da estrutura, conjunto de atributos, valores/classes, tipo de dado, flag de ativo.
- Dados processados: a edição monta dinamicamente a lista de atributos; o tipo `QUALITATIVE` libera classes; `QUANTITATIVE` torna a área de classes somente leitura.
- Dados de saída: gravação em `estrutura`, `estrutura_config` e `atributo`; tabelas de manutenção e confirmação visual.
- Componentes acionados: `uiNewEstrutura`, `uiEditEstrutura`, `selectAllEstrutura`, `selectConfigByEstrutura`, `selectAllAtributosByEstruturas`, `selectAllTipoDados`.
- Lógica interna: o formulário é reconstruído dinamicamente conforme a lista de atributos muda; observers antigos são destruídos para evitar duplicidade de eventos.
- Validações ou regras aplicadas: nome obrigatório; nome não pode duplicar; atributo precisa de nome e tipo; classes só fazem sentido quando o tipo é qualitativo.
- Dependências: `tipo_data`, `estrutura_config`, `atributo`, `Swiper`, `DT`, `actionWebUser`.
- Relação com outros menus: alimenta `Objeto`, `Alarme` e os fluxos de contexto/training.
- Impacto no fluxo geral: define a semântica dos dados capturados e seu papel no alarme e no treino.
- Justificativa arquitetural: estrutura separa “o que o objeto é” do “como ele é observado”.

## Alarme
- Objetivo do menu: criar regras de alarme sobre atributos estruturados.
- Problema que resolve: permite transformar observação em evento de negócio baseado em condições lógicas.
- Contexto de uso: usado quando a operação precisa de alertas por comportamento, estado ou combinação de atributos.
- Funcionalidades disponíveis: criação, listagem, edição e exclusão de alarmes.
- Ações do usuário: selecionar setor e/ou objeto, definir severidade, escrever descrição, montar a expressão lógica, ativar/desativar e salvar.
- Dados de entrada: escopo, nome, descrição, severidade, flag de ativo e expressão JSON.
- Dados processados: o builder monta grupos, regras, operadores e valores; os campos disponíveis vêm da estrutura; o JSON final é serializado para persistência.
- Dados de saída: registros em `alarme` e, opcionalmente, `alarme_evento`.
- Componentes acionados: `uiNewAlarme`, `uiListAlarmes`, `listAlarmFields`, `selectAllAlarmes`, `insertNewAlarme`, `updateAlarme`, `deleteAlarme`.
- Lógica interna: o construtor usa grupos com `AND/OR`, regras com operadores e validação de tipo textual/numeric; a expressão final é gerada como JSON.
- Validações ou regras aplicadas: nome obrigatório; JSON obrigatório; severidade entre `INFO`, `LOW`, `MEDIUM`, `HIGH`, `CRITICAL`; campos e operadores precisam ser coerentes com o tipo de dado.
- Dependências: estrutura e atributos existentes, `jsonlite`, `DT`, `actionWebUser`.
- Relação com outros menus: depende diretamente de `Estrutura`, `Objeto` e `Setor`.
- Impacto no fluxo geral: converte dados estruturados em condição operacional e histórico de evento.
- Justificativa arquitetural: separa a definição da regra de alarme da captura dos dados que a alimentam.

## Treinar
- Objetivo do menu: organizar pacotes de treino e gerar datasets a partir de frames e contexto.
- Problema que resolve: automatiza a transformação de dados visuais e contextuais em artefatos reaproveitáveis.
- Contexto de uso: usado quando a operação precisa montar um conjunto de treino/validação/teste.
- Funcionalidades disponíveis: criação de pacote, montagem de clip, rastreamento de componentes, geração de build e exportação em `.rds`.
- Ações do usuário: selecionar setor e objeto, escolher período, carregar frames, desenhar/editar regiões, criar pacotes, listar pacotes, compactar build, escolher diretório e nome do arquivo.
- Dados de entrada: setor, objeto, intervalo temporal, frames, polígonos/retângulos, tipo de pacote, saída desejada.
- Dados processados: o sistema busca frames no banco, associa câmeras ao objeto, prepara payload com componentes e frames, valida presença de frames em todas as câmeras vinculadas, e monta o pacote de saída.
- Dados de saída: pacotes em `pacote_ia`, datasets consolidados em `.rds`, e material auxiliar para treino.
- Componentes acionados: `uiNewTreinar`, `uiBuildTreinar`, `selectAllTypesPacote`, `selectAllPacoteToTraino`, `selectBuildTreinoPacotes`, `selectBuildTreinoSummary`, `buildTreinoDatasetRow`, `buildTreinoFileName`, `fetch_frames_blob`, `db_fetch_frame_by_id`, `db_fetch_many_frames_by_id`, `db_fetch_frame_raw`, `db_fetch_many_frames`.
- Lógica interna: a criação de pacote usa player temporal, clips, overlays e tracking; o build usa seleção de pacotes, nomeação sanitizada, diretório resolvido e compactação final.
- Validações ou regras aplicadas: setor e objeto precisam existir; o período deve ser válido; o objeto precisa de câmeras associadas; pacotes sem frames suficientes são ignorados no build; o nome do arquivo `.rds` é normalizado.
- Dependências: `mirai`, `ffmpeg`, `magick`, `leaflet`, `DT`, `shinyWidgets`, `base64enc`, banco com frames e blobs.
- Relação com outros menus: depende de `Camera`, `Objeto`, `Setor` e, indiretamente, de `Estrutura`.
- Impacto no fluxo geral: é o ponto de saída para uso analítico posterior e reprodutibilidade de treino.
- Justificativa arquitetural: encapsula a passagem do mundo operacional para o mundo de dados preparados.

Observação de completude do snapshot:
- O roteamento em `app/view/init.R` referencia `uiEditAlarme` e `uiEditTreinar`, mas essas funções não apareceram no conjunto de arquivos analisado.
- O que é documentável com segurança no snapshot é `uiNewAlarme`, `uiListAlarmes`, `uiNewTreinar` e `uiBuildTreinar`.
- O submenu `Agenda` de Setor está comentado no router e não faz parte do fluxo ativo.
- `Dashboard`, `Chat`, `Flow Graphic`, `Video Clip`, `Brain` e `Plot` existem como módulos no repositório, mas não estão montados como menus ativos no sidebar atual.

## Módulos auxiliares não expostos no sidebar atual

### Dashboard
- Objetivo: oferecer leitura analítica do período, com resumo executivo, filtros e atualização assíncrona.
- Papel: agrega dados de contexto e interage com `flow_graphic` e `video_clip`.
- Estado atual: o módulo existe, mas o `app/view/init.R` não o monta como menu ativo.

### Chat
- Objetivo: atuar como assistente de consulta e geração de relatórios.
- Papel: recebe mensagens, mantém contexto, gera texto, gráficos e links de relatório.
- Dependência explícita: `OPENAI_API_KEY` aparece no módulo.
- Estado atual: existe como módulo, mas não está acoplado ao sidebar principal.

### Flow Graphic
- Objetivo: exibir fluxo vertical de setores e máquinas com mini-Gantt por máquina.
- Papel: conectar estrutura hierárquica e episódios temporais.
- Estado atual: é um painel auxiliar usado por variantes de dashboard, não um menu principal.

### Video Clip
- Objetivo: gerar MP4 a partir de frames em processo assíncrono.
- Papel: compor vídeo a partir de intervalos temporais e múltiplas câmeras.
- Dependências: `ffmpeg`, `mirai`, blobs de frames, `magick` para conversão quando necessário.

### Brain
- Objetivo: visualizar frames em sequência com controle manual.
- Papel: é um app standalone de inspeção temporal.
- Estado atual: aparece como script isolado e não como módulo do fluxo principal.

### Plot
- Objetivo: fornecer leitura de metadados e configuração de gráficos por setor.
- Papel: é uma camada legada/auxiliar para consultas gráficas.
- Estado atual: o código existe, mas não está montado no sidebar do app principal.

# 6. Fluxo Funcional do Sistema
O fluxo funcional do sistema começa antes da navegação de menus.

1. O usuário inicia a aplicação.
2. O sistema mostra um modal de login/configuração de banco.
3. O usuário informa host, porta, banco, usuário e senha.
4. O sistema valida os dados e testa a conexão.
5. Quando a conexão é validada, o pool é inicializado e a interface principal é liberada.
6. O sidebar é carregado com menus, cada um acionando um modal próprio.
7. Ao selecionar um submenu, o sistema dispara `actionWebUser`, mostra loader e abre o fluxo correspondente.
8. O usuário preenche dados, aciona salvar, editar ou excluir.
9. O menu chama o DAO adequado, que executa transações, validações e persistência.
10. A UI atualiza a tabela, fecha o modal e volta ao estado `noop`.

Em termos de navegação, o sistema não funciona como um conjunto de páginas independentes. Ele funciona como uma sequência de módulos modais com estado por sessão. Isso importa porque:
- os observers são criados e destruídos por ciclo de vida;
- o menu não precisa permanecer “aberto” depois da ação;
- o foco do usuário fica no fluxo em execução.

Os módulos são acionados em ordem conforme necessidade:
- cadastro base primeiro: câmera, setor, estrutura;
- depois vinculação e modelagem: objeto;
- depois expressão de negócio: alarme;
- depois geração de material: treino;
- depois leitura analítica ou relatórios, quando os módulos auxiliares são usados.

# 7. Regras de Negócio
As regras mais relevantes observadas no projeto são:

- login não é autenticação de usuário no sentido clássico; o fluxo atual valida a conexão com o banco.
- o pool de banco é compartilhado por sessão e não deve ser trocado se houver outra sessão ativa com configuração diferente.
- nomes de entidades principais precisam ser únicos, conforme a validação dos DAOs.
- câmera exige nome e URL únicos.
- setor exige nome único e parâmetros temporais obrigatórios.
- estrutura exige nome único.
- objeto exige nome único e vínculo com setor; o tipo do objeto altera o comportamento visual e de persistência.
- atributos de estrutura usam tipo de dado para decidir se o campo de classes fica editável.
- alarmes dependem de atributos estruturados e expressão JSON.
- pacotes de treino dependem de frames existentes em todas as câmeras associadas ao objeto.
- o build de treino ignora pacotes que não têm cobertura completa de frames.
- os intervalos temporais são tratados em UTC no banco e convertidos para o fuso local na UI quando necessário.
- a retenção de frames é automática via evento do banco, mantendo aproximadamente 24 horas.
- o sidebar sempre retorna para `noop` após abrir um modal de edição/criação.
- observer e modal têm ciclo de vida controlado; ao fechar, o estado da sessão é limpo.

# 8. Componentes Internos e Responsabilidades
Os componentes centrais do sistema são os seguintes:

- `app/main.R`: exporta `ui` e `server` para o Rhino e concentra o boot da aplicação.
- `app/view/init.R`: monta o layout principal, integra login, sidebar, header, dropdowns e roteamento dos menus.
- `app/view/login.R`: coleta e valida a configuração do banco e libera o acesso à sessão.
- `app/view/global.R`: concentra utilitários de UI, notificações, loaders, alertas, DataTable e manipulação de DOM.
- `app/view/model.R`: encapsula modais e confirmações com comportamento uniforme.
- `app/model/Swiper.R`: implementa navegação por slides dentro de modais.
- `app/infra/db_pool.R`: cria e gerencia o pool de conexão e o ciclo de vida das sessões.
- `app/infra/database.R`: fornece helpers transacionais e operações genéricas de SQL no padrão Postgres.
- `app/logic/*.R`: implementa consultas e regras de acesso para cada entidade do domínio.
- `app/view/camera.R`, `setor.R`, `objeto.R`, `estrutura.R`, `alarme.R`, `treinar.R`: são as telas funcionais do domínio.
- `app/view/dashboard.R`, `chat.R`, `flow_graphic.R`, `video_clip.R`: são módulos auxiliares de análise, IA, visualização e geração de mídia.
- `app/view/themes.R`: define tema visual do dashboard.
- `tests/`: cobre ao menos smoke tests e e2e básico.
- `document/`: declara o contrato do banco de dados.
- `train/`: armazena datasets e pacotes já preparados.

A razão de essa organização existir é reduzir mistura de responsabilidades. O código de UI não decide regra de negócio sozinho; a UI pergunta à lógica. A lógica não gerencia ciclo de modal; isso é função da view e dos observers. A infraestrutura não sabe “o que é uma câmera”; ela só sabe abrir, manter e fechar conexões.

# 9. Lógica de Processamento
O processamento interno segue uma linha consistente.

Recebimento:
- inputs chegam pela UI reativa;
- eventos `observeEvent` capturam cliques, mudanças e confirmações.

Tratamento:
- campos são normalizados para maiúsculas ou formatos de data;
- validações verificam vazio, duplicidade e coerência temporal;
- em alguns fluxos, o conteúdo é convertido para JSON ou para dataframes longos;
- o sistema decide se vai abrir edição, criar novo registro, atualizar ou excluir.

Encaminhamento:
- a ação vai para um DAO específico;
- o DAO faz `SELECT`, `INSERT`, `UPDATE` ou `DELETE`;
- quando necessário, as operações rodam dentro de `db$tryTransaction`.

Retorno:
- a resposta do banco atualiza a tela;
- o modal é fechado ou avança de slide;
- o sistema exibe `showNotification`, confirmações ou overlays.

Onde as decisões acontecem:
- no login, para validar a conexão;
- no router do sidebar, para distinguir “Novo”, “Lista” e “Contexto”;
- nos validadores dos DAOs, para bloquear duplicidade;
- no treino, para decidir se um pacote pode ou não entrar no build;
- no alarme, para montar a árvore lógica de regras;
- no objeto, para escolher entre fluxo estático e dinâmico.

Em resumo, a lógica de processamento é orientada a evento e sessão. Não há um motor único central fazendo tudo; há módulos especializados que compõem um fluxo maior.

# 10. Integrações e Dependências
As integrações e dependências mais relevantes são:

- Banco de dados PostgreSQL via `RPostgres` e `pool`.
- Esquema relacional definido em `document/script.sql`.
- Dependência de `event_scheduler` no banco para retenção de frames.
- JS client-side com `swiper`, `main.js` e scripts auxiliares em `app/js/`.
- UI baseada em `shiny`, `shinydashboard`, `shinydashboardPlus`, `shinyjs`, `shinyWidgets`, `DT`, `plotly`, `visNetwork`, `leaflet`, `leaflet.extras`, `htmlwidgets`, `shinycssloaders`.
- Processamento de mídia com `ffmpeg` e, quando necessário, `magick`.
- Processamento assíncrono com `mirai` e agendamento com `later`.
- Serialização e manipulação de contexto com `jsonlite`.
- Caching em memória com `cachem` e chaveamento com `digest`.
- Integração opcional com OpenAI no módulo de chat/relatórios por meio de `OPENAI_API_KEY`.
- Ambiente reprodutível com `renv.lock`.

Variáveis de ambiente relevantes:
- `DB_NAME`
- `DB_HOST`
- `DB_PORT`
- `DB_USER`
- `DB_PASS`
- `DB_IDLE_TIMEOUT`
- `TVS_MAX_ASYNC`
- `TVS_MAX_VCLIP`
- `TVS_MIRAI_DAEMONS`
- `TVS_ADMIN_USER`
- `TVS_ADMIN_PASS`
- `TVS_AUTH_SALT`
- `OPENAI_API_KEY`
- `USE_CACHE`
- `RHINO_LOG_LEVEL`
- `RHINO_LOG_FILE`

Integrações legadas ou fora do fluxo principal:
- `app/view/brain.R` ainda usa uma conexão MariaDB standalone com credenciais fixas no script.
- alguns arquivos históricos em `app/logic/` e `document/struct.sql` preservam sintaxe e estrutura da fase anterior do projeto.
- o repositório contém um `README.md` mínimo, mas ele não documenta o sistema com profundidade.

# 11. Objetivo Funcional de Cada Parte do Sistema
- Login: garantir que a sessão tenha um banco válido antes de qualquer operação.
- Sidebar e router: transformar o sistema em fluxos guiados e previsíveis.
- Camera: dar origem ao dado visual e ao índice temporal de captura.
- Setor: dar contexto operacional e janela temporal ao restante das entidades.
- Objeto: definir o que é observado e como isso se relaciona a setor, câmera, geometria e contexto.
- Estrutura: definir a semântica de atributos e como o objeto deve ser interpretado.
- Alarme: converter atributos em condição operacional e evento.
- Treinar: transformar contexto e frames em dataset utilizável.
- Dashboard/Flow: ler a operação e traduzi-la em visão executiva.
- Chat: permitir consulta assistida e produção de relatórios a partir de contexto e resultados.
- Video Clip: gerar recortes de vídeo a partir de janelas de interesse.
- Infraestrutura: manter conexão, transação e integridade.
- Componentes de UI: evitar repetição e manter padrão visual e funcional.
- Banco: armazenar a verdade operacional e as relações entre entidades.
- Testes: verificar pelo menos a existência do fluxo base e a abertura do app.

# 12. Relacionamento Entre Menus, Módulos e Fluxos
A visão conectada do sistema é esta:

- `Camera` alimenta `Objeto`, `Treinar`, `Dashboard`, `Flow Graphic` e qualquer módulo que precise recuperar frames.
- `Setor` alimenta `Camera`, `Objeto`, `Estrutura`, `Alarme`, `Treinar` e relatórios por escopo.
- `Estrutura` alimenta `Objeto` e `Alarme`, porque define os atributos e a semântica das regras.
- `Objeto` alimenta `Treinar`, `Chat`, `Dashboard` e `Flow Graphic`, porque gera contexto temporal e relacionamento com câmeras.
- `Alarme` depende de `Estrutura` e de `Objeto` para construir expressões válidas.
- `Treinar` depende de `Camera` e `Objeto` para localizar frames e montar pacotes.
- `Dashboard` depende de `Objeto`, `Setor` e do intervalo temporal para compor os resumos.
- `Flow Graphic` depende de `Objeto` e de episódios temporais para desenhar a hierarquia e o Gantt.
- `Video Clip` depende de frames do banco e é acionado pelos módulos analíticos quando há necessidade de recorte.
- `Chat` depende do contexto construído pelo restante do sistema para gerar texto, gráficos e relatórios.

Os pontos de entrada são:
- login do banco;
- sidebar do `app/view/init.R`.

Os pontos de transformação são:
- DAOs de cada entidade;
- helpers de contexto;
- geração de expressão JSON;
- montagem de payload de treino;
- conversão de frames em vídeo.

Os pontos de saída são:
- registros no banco;
- tabelas de consulta;
- alertas e notificações;
- relatórios HTML/PDF;
- datasets `.rds`;
- recortes de vídeo.

# 13. Conclusão Técnica
O projeto está estruturado como um sistema de visão orientado a domínio, com separação clara entre interface, persistência e regras. A organização atual suporta a operação porque cada entidade tem um módulo de cadastro, um conjunto de validações e uma camada de acesso a dados própria.

Do ponto de vista arquitetural, o sistema é sólido naquilo que já está montado:
- usa Rhino/Shiny como base;
- centraliza o banco em pool e transações;
- controla ciclo de vida de modais e observers;
- separa componentes reutilizáveis;
- preserva um contrato relacional explícito em SQL;
- inclui processamento assíncrono para tarefas pesadas.

Do ponto de vista funcional, o conjunto de menus cobre o ciclo operacional essencial:
- cadastrar a infraestrutura visual;
- modelar o que será observado;
- estruturar o significado dos atributos;
- definir alarmes;
- preparar datasets de treino;
- visualizar e consultar o comportamento do sistema.

Há também módulos auxiliares importantes que ampliam o ecossistema do projeto, mesmo não estando montados como menus no sidebar atual. Eles mostram que o sistema foi pensado para crescer além do CRUD inicial e para sustentar análise, relatórios, fluxo visual e geração de mídia.

Em síntese, o Telemetry Vision System é um sistema modular, orientado a domínio e fortemente ligado a dados temporais e visuais. O projeto já expõe uma arquitetura compreensível, com responsabilidades bem definidas entre menus, módulos, banco e processamento, o que permite que outro desenvolvedor entenda a lógica sem depender do código e que um agente futuro replique a estrutura em outra stack com base neste contrato funcional.