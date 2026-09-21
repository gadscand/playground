namespace CClassTrib

/-- https://taxcel.com.br/cclass-cst-ibs-cbs --/

/-- Estados brasileiros. --/
inductive UF where
  | AC | AL | AP | AM | BA | CE | DF | ES
  | GO | MA | MT | MS | MG | PA | PB | PR
  | PE | PI | RJ | RN | RS | RO | RR | SC
  | SP | SE | TO
  deriving Repr, DecidableEq


/-- Tipo de operação. --/
inductive OperationType where
  | sale
  | return
  | transfer
  | import
  | export
  deriving Repr, DecidableEq


/-- Finalidade da compra. --/
inductive Purpose where
  | resale
  | consumption
  | industrialization
  | fixedAsset
  deriving Repr, DecidableEq


/-- Regime especial. --/
inductive SpecialRegime where
  | none
  | simpleNational
  | zonaFranca
  | ruralProducer
  | cooperative
  deriving Repr, DecidableEq


/-- Tipo do comprador. --/
inductive BuyerType where
  | individual
  | company
  | government
  | ruralProducer
  deriving Repr, DecidableEq


/-- Tipo do vendedor. --/
inductive SellerType where
  | individual
  | company
  | ruralProducer
  deriving Repr, DecidableEq


/-- Origem da mercadoria. --/
inductive ProductOrigin where
  | domestic
  | imported
  deriving Repr, DecidableEq


/-- NCM. Para a demonstração. --/
abbrev NCM := String

/-- Código CClassTrib. --/
abbrev CClassTribCode := String


/-!
  FATOS

  O ponto fundamental:

      Option X

  significa:

      none     = ainda não sabemos
      some x   = sabemos
-/

structure Facts where
  ncm             : Option NCM := none
  origin          : Option ProductOrigin := none

  sellerUF        : Option UF := none
  buyerUF         : Option UF := none

  sellerType      : Option SellerType := none
  buyerType       : Option BuyerType := none

  operation       : Option OperationType := none
  purpose         : Option Purpose := none

  specialRegime   : Option SpecialRegime := none

  finalConsumer   : Option Bool := none

  deriving Repr


def Facts.empty : Facts :=
  {}


inductive Question where
  | ncm
  | origin
  | sellerUF
  | buyerUF
  | sellerType
  | buyerType
  | operation
  | purpose
  | specialRegime
  | finalConsumer
  deriving Repr, DecidableEq


def Question.text : Question → String
  | .ncm =>
      "Qual é o NCM do produto?"

  | .origin =>
      "Qual é a origem da mercadoria?"

  | .sellerUF =>
      "Em qual UF está estabelecido o vendedor?"

  | .buyerUF =>
      "Em qual UF está estabelecido o comprador?"

  | .sellerType =>
      "Qual é o tipo do vendedor?"

  | .buyerType =>
      "Qual é o tipo do comprador?"

  | .operation =>
      "Qual é o tipo da operação?"

  | .purpose =>
      "Qual é a finalidade da aquisição?"

  | .specialRegime =>
      "Existe algum regime especial aplicável?"

  | .finalConsumer =>
      "O destinatário é consumidor final?"

/- Teste de decisão --/
inductive Decision where

  /-- Ainda precisamos perguntar alguma coisa. --/
  | ask : Question → Decision

  /-- Temos informação suficiente para classificar. --/
  | classify : CClassTribCode → Decision

  /-- Nenhuma regra se aplica. --/
  | noMatch : Decision

  /-- Mais de uma regra aplicável. --/
  | conflict : List CClassTribCode → Decision

  deriving Repr

/--
  Resultado de uma regra individual.

  Uma regra pode dizer:

    notApplicable
    need question
    classify code
-/

inductive RuleResult where
  | notApplicable
  | need : Question → RuleResult
  | classify : CClassTribCode → RuleResult
  deriving Repr

/--
  Regra de demonstração para arroz.

  Ela representa apenas:

    NCM começa com 1006
    +
    operação = venda
    +
    operação interestadual
    +
    regime especial = nenhum

  => CClassTrib.
-/

def riceRule (f : Facts) : RuleResult :=
  match f.ncm with

  | none =>
      .need .ncm

  | some ncm =>

      if ncm.startsWith "1006" then

        match f.operation with

        | none =>
            .need .operation

        | some op =>

            match op with

            | OperationType.sale =>

                match f.sellerUF, f.buyerUF with

                | none, _ =>
                    .need .sellerUF

                | _, none =>
                    .need .buyerUF

                | some seller, some buyer =>

                    if seller == buyer then
                      .notApplicable

                    else

                      match f.specialRegime with

                      | none =>
                          .need .specialRegime

                      | some SpecialRegime.none =>
                          .classify "200003"

                      | some _ =>
                          .notApplicable

            | _ =>
                .notApplicable

      else
        .notApplicable


/--
  Segunda regra fictícia.

  Serve para demonstrar que o sistema pode possuir
  múltiplas regras.
-/

def domesticConsumptionRule (f : Facts) : RuleResult :=
  match f.ncm, f.operation, f.purpose with

  | some ncm,
    some OperationType.sale,
    some Purpose.consumption =>

      if ncm.startsWith "1006" then
        .classify "200003"

      else
        .notApplicable

  | _, _, _ =>
      .notApplicable


/--
  Lista das regras existentes.
-/
structure Rule where
  name        : String
  evaluate    : Facts → RuleResult
  description : String


def rules : List Rule :=
  [
    {
      name := "rice-interstate-sale"

      evaluate := riceRule

      description :=
        "Regra para venda interestadual de arroz."
    },

    {
      name := "rice-domestic-consumption"

      evaluate := domesticConsumptionRule

      description :=
        "Regra fictícia para consumo de arroz."
    }
  ]


/--
  Avalia todas as regras.
-/
def evaluateRules
    (f : Facts)
    (rs : List Rule) : List RuleResult :=
  rs.map (fun r => r.evaluate f)


/--
  Extrai códigos de classificação.
-/
def classifications :
    List RuleResult →
    List CClassTribCode

  | [] =>
      []

  | RuleResult.classify code :: xs =>
      code :: classifications xs

  | _ :: xs =>
      classifications xs


/--
  Encontra a primeira pergunta necessária.
-/
def firstQuestion :
    List RuleResult →
    Option Question

  | [] =>
      none

  | RuleResult.need q :: _ =>
      some q

  | _ :: xs =>
      firstQuestion xs


/--
  Decide o que fazer.
-/
def runDecide
    (f : Facts)
    (rs : List Rule) : Decision :=

  let results := evaluateRules f rs

  let codes := classifications results

  match codes with

  | [] =>

      match firstQuestion results with

      | some q =>
          .ask q

      | none =>
          .noMatch

  | [code] =>
      .classify code

  | codes =>
      .conflict codes


/--
  Evidência humana-legível associada à classificação.
-/
structure Evidence where
  ruleName : String
  code : CClassTribCode
  facts : Facts
  explanation : String

  deriving Repr


/--
  Constrói uma evidência para a regra do arroz.
-/
def riceEvidence (f : Facts) : Option Evidence :=

  match f.ncm,
        f.operation,
        f.sellerUF,
        f.buyerUF,
        f.specialRegime with

  | some ncm,
    some OperationType.sale,
    some seller,
    some buyer,
    some SpecialRegime.none =>

      if ncm.startsWith "1006" && seller != buyer then

        some {
          ruleName := "rice-interstate-sale"
          code := "200003"
          facts := f

          explanation :=
            "NCM inicia com 1006, operação é venda, " ++
            "vendedor e comprador estão em UFs diferentes " ++
            "e não existe regime especial informado."
        }

      else
        none

  | _, _, _, _, _ =>
      none


/-!
    Estado do sistema.
-/

structure Conversation where
  facts : Facts
  history : List Question := []

  deriving Repr


def Conversation.initial : Conversation :=
  {
    facts := Facts.empty
    history := []
  }


/--
  Registra uma pergunta no histórico.
-/
def Conversation.recordQuestion
    (c : Conversation)
    (q : Question) : Conversation :=
  {
    c with
    history := q :: c.history
  }


/--
  Respostas possíveis.
-/

def Conversation.answerNCM
    (c : Conversation)
    (value : NCM) : Conversation :=
  {
    c with
    facts := { c.facts with ncm := some value }
  }


def Conversation.answerSellerUF
    (c : Conversation)
    (value : UF) : Conversation :=
  {
    c with
    facts := { c.facts with sellerUF := some value }
  }


def Conversation.answerBuyerUF
    (c : Conversation)
    (value : UF) : Conversation :=
  {
    c with
    facts := { c.facts with buyerUF := some value }
  }


def Conversation.answerOperation
    (c : Conversation)
    (value : OperationType) : Conversation :=
  {
    c with
    facts := { c.facts with operation := some value }
  }


def Conversation.answerPurpose
    (c : Conversation)
    (value : Purpose) : Conversation :=
  {
    c with
    facts := { c.facts with purpose := some value }
  }


def Conversation.answerSpecialRegime
    (c : Conversation)
    (value : SpecialRegime) : Conversation :=
  {
    c with
    facts := { c.facts with specialRegime := some value }
  }


/--
  Executa o motor de decisão sobre a conversa.
-/
def Conversation.decide
    (c : Conversation) : Decision :=
  runDecide c.facts rules

/--
Funções de retorno, na prática, o stdout seria para o sistema.
-/
def printDecision : Decision → IO Unit

  | .ask q => do
      IO.println "DECISÃO: informação insuficiente"
      IO.println s!"PERGUNTA: {Question.text q}"

  | .classify code => do
      IO.println "DECISÃO: classificação encontrada"
      IO.println s!"cClassTrib = {code}"

  | .noMatch => do
      IO.println "DECISÃO: nenhuma regra aplicável"

  | .conflict codes => do
      IO.println "DECISÃO: conflito entre regras"
      IO.println s!"Códigos possíveis: {codes}"


/-!
  DEMONSTRAÇÃO
-/

/--
  Simula a conversa:

    Usuário:
      "Qual cClassTrib para venda de arroz SP -> MG?"

  A LLM poderia inicialmente extrair:

      NCM       = desconhecido
      sellerUF  = SP
      buyerUF   = MG
      operation = sale

  Depois o Lean pergunta somente aquilo que falta.
-/

def demo : IO Unit := do

  IO.println ""
  IO.println "   DEMONSTRAÇÃO CCLASSTRIB + LEAN 4"
  IO.println ""

  /-
    PASSO 0
  -/

  IO.println "PASSO 0"
  IO.println "Usuário:"
  IO.println "\"Qual cClassTrib devo usar para venda de arroz SP -> MG?\""
  IO.println ""

  let c0 := Conversation.initial

  IO.println "Fatos conhecidos:"
  IO.println "  vendedor = SP"
  IO.println "  comprador = MG"
  IO.println "  operação = venda"
  IO.println "  produto = arroz (interpretação da LLM)"
  IO.println "  NCM = desconhecido"
  IO.println ""

  let c0 :=
    c0
      |>.answerSellerUF UF.SP
      |>.answerBuyerUF UF.MG
      |>.answerOperation OperationType.sale

  let d0 := c0.decide

  printDecision d0

  IO.println ""


  /-
    PASSO 1
  -/

  IO.println "PASSO 1"
  IO.println "A LLM pergunta ao usuário:"
  IO.println "\"Qual é o NCM do arroz?\""
  IO.println ""

  IO.println "Usuário:"
  IO.println "\"1006.30.21\""
  IO.println ""

  let c1 :=
    c0.answerNCM "1006.30.21"

  let d1 := c1.decide

  printDecision d1

  IO.println ""


  /-
    PASSO 2
  -/

  IO.println "PASSO 2"
  IO.println "O Lean determina que ainda falta:"
  IO.println ""

  match d1 with

  | .ask q =>
      IO.println s!"  {Question.text q}"

  | _ =>
      IO.println "  Nenhuma pergunta."

  IO.println ""

  IO.println "Usuário:"
  IO.println "\"Não existe regime especial.\""
  IO.println ""

  let c2 :=
    c1.answerSpecialRegime SpecialRegime.none

  let d2 := c2.decide

  printDecision d2

  IO.println ""


  /-
    PROVA / EVIDÊNCIA
  -/

  IO.println "EVIDÊNCIA DA CLASSIFICAÇÃO"
  IO.println ""

  match riceEvidence c2.facts with

  | some evidence =>
      IO.println s!"Regra: {evidence.ruleName}"
      IO.println s!"Código: {evidence.code}"
      IO.println ""
      IO.println "Justificativa:"
      IO.println evidence.explanation

  | none =>
      IO.println "Não foi possível construir evidência."

  IO.println ""

  IO.println "FIM"


/-!
  VERIFICAÇÃO FORMAL

  Agora demonstramos que:

  Dadas as condições da regra, o Lean consegue provar
  que a função retorna determinado cClassTrib.
-/

/--
  Predicado que representa as condições da regra fictícia.
-/
def RiceEligible (f : Facts) : Prop :=
  ∃ ncm,
    f.ncm = some ncm ∧
    ncm.startsWith "1006" ∧
    f.operation = some OperationType.sale ∧
    ∃ seller buyer,
      f.sellerUF = some seller ∧
      f.buyerUF = some buyer ∧
      seller ≠ buyer ∧
      f.specialRegime = some SpecialRegime.none


/--
  Teorema:

  Se as condições da regra são satisfeitas,
  então a regra produz o código esperado.

  Este é o tipo de propriedade que queremos aumentar
  progressivamente quando mais regras tributárias
  forem formalizadas.
-/
theorem rice_rule_correct
    {f : Facts}
    (h : RiceEligible f) :
    riceRule f = .classify "EXEMPLO-ARROZ-001" := by

  rcases h with
    ⟨ncm, hncm, hncm1006, hop, seller, buyer,
     hseller, hbuyer, hdiff, hregime⟩

  simp [
    riceRule,
    hncm,
    hncm1006,
    hop,
    hseller,
    hbuyer,
    hdiff,
    hregime
  ]


/-!
  12. EXEMPLO CONCRETO
-/

/--
  Podemos criar um estado concreto e avaliá-lo.
-/
def exampleFacts : Facts :=
  {
    ncm := some "1006.30.21"
    sellerUF := some UF.SP
    buyerUF := some UF.MG
    operation := some OperationType.sale
    specialRegime := some SpecialRegime.none
  }


#eval runDecide exampleFacts rules

#eval
  match riceEvidence exampleFacts with
  | some e => e.code
  | none => "SEM_CLASSIFICACAO"


/-!
    Demonstração
-/

#eval demo


end CClassTrib
