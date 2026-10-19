# EP02 — Query builder funcional em Clojure

**Integrantes:**

- Danilo Hideo Yamamoto
- Kauê de Sousa Ibiapino

## Como executar

```
clojure -M:run dados/filmes.csv
```

Abre um terminal com o prompt `> `. Comandos: `QUERY <consulta>` e `EXIT`. O fim da entrada (por exemplo, vinda de um *pipe*) equivale a `EXIT`:

```
$ printf 'QUERY contar\nQUERY media nota\nEXIT\n' | clojure -M:run dados/filmes.csv
```

Testes automatizados (a bateria `casos_teste_ep02.txt` transcrita como dado, o contrato do builder e o nó desconhecido):

```
clojure -M:test
```

## A forma da AST

Uma consulta é **dado puro**: um mapa com dois campos, feito só de mapas, vetores, palavras-chave, números e *strings*. Não há funções dentro dela.

```clojure
{:estagios [estagio ...]      ; vetor, na ordem do texto
 :terminal nil}               ; nil, ou um nó terminal
```

Nós de **estágio** (vão em `:estagios`):

| Nó | Texto |
|---|---|
| `[:onde expr]` | `onde <expr>` |
| `[:ordenar-por :campo :asc]` / `[:ordenar-por :campo :desc]` | `ordenar por <campo> [desc]` (a ordem é sempre explícita) |
| `[:limitar n]` | `limitar <n>` |
| `[:selecionar [:c1 :c2 ...]]` | `selecionar c1, c2, ...` (tem que ser o último elemento: sem estágio nem terminal depois) |

Nós de **terminal** (vão em `:terminal`):

| Nó | Texto |
|---|---|
| `[:contar]` | `contar` |
| `[:soma :campo]` | `soma <campo>` |
| `[:media :campo]` | `media <campo>` |
| `[:agrupar-por :campo agregacao]` | `agrupar por <campo> com <agregacao>`, onde `agregacao` é `[:contar]`, `[:soma :campo]` ou `[:media :campo]` |

Nós de **expressão** (dentro de `:onde`):

| Nó | Texto |
|---|---|
| `[:= :campo valor]`, `[:!= ...]`, `[:< ...]`, `[:<= ...]`, `[:> ...]`, `[:>= ...]` | `campo op literal` (`valor` é número ou *string*) |
| `[:e a b]` | `a e b` |
| `[:ou a b]` | `a ou b` |
| `[:nao a]` | `nao a` |

`e` e `ou` são binários e associativos à esquerda: `a ou b ou c` vira `[:ou [:ou a b] c]`.

### Exemplo

O texto `onde genero = "drama" e nota >= 8 | ordenar por nota desc | selecionar titulo, nota` vira:

```clojure
{:estagios [[:onde [:e [:= :genero "drama"] [:>= :nota 8]]]
            [:ordenar-por :nota :desc]
            [:selecionar [:titulo :nota]]]
 :terminal nil}
```

Outro, com terminal: `onde ano > 2010 | agrupar por genero com media duracao`:

```clojure
{:estagios [[:onde [:> :ano 2010]]]
 :terminal [:agrupar-por :genero [:media :duracao]]}
```

### Nó desconhecido (usado na correção)

Um estágio que não existe na linguagem é um vetor com outro tipo na primeira posição, por exemplo:

```clojure
(executar {:estagios [[:voar 3]] :terminal nil} dados)
;; => IllegalArgumentException: No matching clause: :voar
```

A execução **falha** (não devolve `{:erros ...}`) com uma exceção cuja mensagem nomeia o nó. O mesmo vale para uma expressão (`[:xor a b]`) ou um terminal (`[:minimo :ano]`) de tipo desconhecido.

### Resultado de `executar`

- sem terminal: sequência de registros (mapas);
- `contar`, `soma`, `media`: o valor (número);
- `agrupar por`: sequência de `{:chave k :valor v}` em ordem crescente de chave;
- consulta inválida contra o esquema: `{:erros ["..."]}`, sem processar nenhum registro.

## O builder

Todas as funções recebem uma consulta e devolvem outra; a de partida não muda.

```clojure
(def base   (consulta))
(def dramas (onde base [:= :genero "drama"]))
(def dois   (limitar dramas 2))
(def tres   (limitar dramas 3))        ; dramas não foi alterada

(= dois (analisar "onde genero = \"drama\" | limitar 2"))   ; => true
```

Além do contrato mínimo (`consulta`, `onde`, `selecionar`, `ordenar-por`, `limitar`, `analisar`, `executar`), há os construtores de terminal `contar`, `soma`, `media` e `agrupar-por`. Acrescentar estágio ou outro terminal a uma consulta que já tem terminal lança `ex-info`; `selecionar` seguido de qualquer coisa só é detectado em `executar` (`{:erros ...}`).

## Os namespaces

| Namespace | Papel |
|---|---|
| `consultas.main` | Único com I/O: lê o arquivo, lê o terminal, imprime. O laço é `loop`/`recur`. Interpreta `QUERY` e `EXIT`. |
| `consultas.sintaxe` | Texto → AST. *Tokenizador* e analisador recursivo descendente: cada função recebe os *tokens* e devolve `[nó tokens-restantes]`. Erros são `ex-info`. |
| `consultas.compilador` | `conferir` (AST × esquema → mensagens de erro) e `compilar` (AST → uma função). Os dois despacham com `case` sobre o tipo do nó, sem ramo padrão. Estágios com argumento são aplicação parcial; a consulta é a composição dos estágios (`reduce` a partir de `identity`). |
| `consultas.core` | Leitor de CSV (texto → esquema e registros), o builder, `analisar` e `executar`. É o que os testes da correção importam. |
| `consultas.saida` | Puro: formata valores (`8.60`, ponto decimal em qualquer localidade), linhas de registro e de grupo, `(vazio)`, e `responder` (texto da consulta → texto da resposta, nunca lança). |

## Decisões de projeto

- **Nenhum nome de campo no código.** Campos, tipos e ordem das colunas vêm do cabeçalho do CSV; a ordem da saída vem do esquema (ou do `selecionar`), não da ordem das chaves do mapa.
- **Números.** `inteiro` é `Long`, `decimal` é `Double`. Comparações usam `compare`, que trata `8` e `8.0` como iguais. `soma` de campo decimal sem registros dá `0.00`; de inteiro, `0`.
- **Ordenação estável.** `sort-by` é estável; no `desc` inverte-se o comparador, não o resultado.
- **Palavras da linguagem pela posição.** `onde`, `e`, `ou`, `nao`, `desc`, `por`, `com` são reconhecidas pela posição na gramática, então um campo chamado, por exemplo, `media` continua utilizável dentro de uma expressão.
- **Erros.** A consulta é conferida contra o esquema antes de executar. Só o primeiro motivo é impresso, numa linha `ERRO: ...`. Qualquer exceção na resposta a um `QUERY` (inclusive excesso de aninhamento) vira `ERRO:`; o programa nunca imprime *stack trace*.
