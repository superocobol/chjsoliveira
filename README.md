Esta é uma série de programas para cadastro de Clientes e vendedores.

1)  CBL_MENU.CBL - Menu Principal e Sub menu
2)  CBL_MENU_CLIENTE.cbl - CRUD de inclusão de Clientes e importação de Clientes
3  CBL_MENU_VENDEDOR.CBL - CRUD de inclusão de Vendedores e importação de Vendedores
4)  CBL_MENU_DISTRIBUICAO.cbl - Executa processo de distribuição de vendedores para cada cliente. Gera CSV.
5)  CBL_CALC_DISTANCIA.cbl - Subprograma para calculo de distância em KM a partir de determinada latitude e longitude
6)  CBL_MENU_REL_CLI.cbl - relatório de clientes (Incompleto)
7)  CBL_MENU_REL_VEN.cbl - relatório de Vendedores (Incompleto)
8)  CPY_CALC_DISTANCIA.CPY - copybook do subprograma de calculo de distância
9)  CPY_ID_ARQ_CLIENTE.CPY - copybook d com informações do caminho do arquivo de clientes quue por padrão é gerado no  diretório C:\code\cobol\db\
10)  CPY_ID_ARQ_VENDEDOR.CPY  - copybook d com informações do caminho do arquivo de vendedores quue por padrão é gerado no  diretório C:\code\cobol\db\
11)  CPY_ID_ARQ_CSV.CPY   - copybook d com informações do caminho do arquivo de CSV gerado na distribuição quue por padrão é gerado no  diretório C:\code\cobol\db\
12)  FS-ARQ-CLIENTE.CPY - layout do arquivo de clientes
13)  FS-ARQ-VENDEDOR.CPY - layout do arquivo de vendedores

Instruções:

1) Criar o diretório C:\code\cobol\db\
2) Compilar os módulos na sequencia 6) 7) 5) 4) 3) 2) e por ultimo executar o CBL_MENU.
3) Os arquivos de cliente e vendedores serão gerados no diretório C:\code\cobol\db\
4) para realizar a importação o layout do arquivo de clientes deve ter o seguinte formato:

    Código Cliente    - pic  9(007);
    CNPJ              - pic  9(014);
    Razão Social      - pic  x(040);
    Latitude          - pic s9(003)v9(008);
    Longitude         - pic s9(003)v9(008);

Com:
             organization       is indexed
             access mode        is dynamic
             lock mode          is manual
5) para realizar a importação o layout do arquivo de vendedores deve ter o seguinte formato:

    Código Vendedor   - pic  9(003);
    CPF               - pic  9(011);
    Nome Vendedor     - pic  x(040);
    Latitude          - pic s9(003)v9(008);
    Longitude         - pic s9(003)v9(008);

Com:
             organization       is indexed
             access mode        is dynamic
             lock mode          is manual

Pendências:

1) Validação de CPF e CNPJ
2)Adicionar paginação, ordernação e filtros aos relatórios de vendedores e clientes



