type openArrayParams* = openarray[tuple[k: string, t: NimNode]]

proc newImport*(id: string): NimNode =
  result = newNimNode(nnkImportStmt)
  result.add ident(id)

proc newInclude*(id: string): NimNode =
  result = newNimNode(nnkIncludeStmt)
  result.add ident(id)

proc newExclude*(id: string): NimNode =
  result = newNimNode(nnkExportStmt)
  result.add ident(id)

proc newWhenStmt*(whenBranch: tuple[cond, body: NimNode]): NimNode =
  ## Constructor for `when` statements.
  result = newNimNode(nnkWhenStmt)
  result.add(newTree(nnkElifBranch, whenBranch.cond, whenBranch.body))

proc newWhenStmt*(whenBranch: tuple[cond, body: NimNode], elseBranch: NimNode): NimNode =
  ## Constructor for `when` statements.
  result = newNimNode(nnkWhenStmt)
  result.add(newTree(nnkElifBranch, whenBranch.cond, whenBranch.body))
  result.add(newTree(nnkElse, elseBranch))

proc newCaseStmt*(caseNode: NimNode, branches: openarray[tuple[cond, body: NimNode]],
         elseBranch = newEmptyNode()): NimNode =
  result = newNimNode(nnkCaseStmt)
  result.add caseNode
  for branch in branches:
    result.add(newNimNode(nnkOfBranch).add( branch.cond, branch.body))
  if elseBranch.kind != nnkNone:
    result.add(newTree(nnkElse, elseBranch))

proc newExceptionStmt*(exception: NimNode, msg: NimNode): NimNode =
  expectKind(exception, nnkIdent)
  expectKind(msg, nnkStrLit)
  result = newNimNode(nnkRaiseStmt)
  result.add(
    newCall(
      ident "newException",
      ident exception.strVal,
      newLit msg.strVal
    )
  )

proc newObject*(id: string, fields: openArrayParams = [], parent = "", public = false): NimNode =
  ## Create a new object
  # result = newNimNode nnkTypeSection
  var fieldDefs = newEmptyNode()
  if fields.len != 0:
    fieldDefs = newNimNode nnkRecList
    for f in fields:
      fieldDefs.add(nnkIdentDefs.newTree(ident f.k, f.t, newEmptyNode()))
  let objectIdent =
    if public: nnkPostfix.newTree(ident "*", ident id)
       else: ident(id)
  let fromParent =
    if parent.len != 0: nnkOfInherit.newTree(ident parent)
            else: newEmptyNode()      
  result =
    nnkTypeDef.newTree(
      objectIdent,
      newEmptyNode(),
      nnkObjectTy.newTree(
        newEmptyNode(),
        fromParent,
        fieldDefs 
      )
    )

proc newTupleType*(id: string, fields: openArrayParams, public = false): NimNode =
  ## Creates a new tuple type
  # result = newNimNode nnkTypeSection
  let tupleIdent =
    if public: nnkPostfix.newTree(ident "*", ident id)
       else: ident id
  var tupleIdentDefs = newEmptyNode()
  if fields.len != 0:
    tupleIdentDefs = nnkTupleTy.newTree()
    for field in fields:
      if field.k.contains("|"):
        var defs = nnkIdentDefs.newTree()
        for f in field.k.split("|"):
          defs.add(ident(f))
        defs.add(field.t)
        defs.add(newEmptyNode())
        tupleIdentDefs.add(defs)
      else:
        tupleIdentDefs.add(
          nnkIdentDefs.newTree(
            ident field.k,
            field.t,
            newEmptyNode()
          )
        )

  result = 
    nnkTypeDef.newTree(
      tupleIdent,
      newEmptyNode(),
      tupleIdentDefs
    )

proc newProc*(id: string, params: openarray[tuple[k,t: string, vt: bool]],
      public = false, returnType, body = newEmptyNode()): NimNode =
  ## Create a new procedure
  result = newNimNode nnkProcDef
  let procIdent = 
    if public: nnkPostfix.newTree(ident "*", ident id)
       else: ident id
  var formalParamsNode = nnkFormalParams.newTree()
  if returnType.kind != nnkNone:
    formalParamsNode.add returnType
  if params.len == 0 and returnType.kind == nnkNone:
    formalParamsNode.add(newEmptyNode())
  else:
    for param in params:
      let valType =
        if param.vt:
          nnkVarTy.newTree(ident(param.t))
        else: ident(param.t)
      formalParamsNode.add(
        nnkIdentDefs.newTree(
          ident param.k,
          valType,
          newEmptyNode()
        )
      )
  result.add(
    procIdent,
    newEmptyNode(),
    newEmptyNode(),
    formalParamsNode,
    newEmptyNode(),
    newEmptyNode(),
    newStmtList(body)
  )

proc newTemplate*(id: string, params: openarray[tuple[k,t: string, vt: bool]],
        public = false, body = newEmptyNode()): NimNode =
  ## Create a template
  ## TODO support for generic parameters
  result = newNimNode nnkTemplateDef
  let templateIdent =
    if public: nnkPostfix.newTree(ident "*", ident id)
       else: ident id
  var formalParams = newEmptyNode()
  if params.len != 0:
    formalParams = newNimNode nnkFormalParams
    formalParams.add newEmptyNode()
    for param in params:            # todo support for mmutable params
      formalParams.add(
        nnkIdentDefs.newTree(
          ident param.k,      # param ident
          ident param.t,      # param type
          newEmptyNode()
        )
      )
  result.add(
    templateIdent,
    newEmptyNode(),
    newEmptyNode(),
    formalParams,
    newEmptyNode(),
    newEmptyNode(),
    newStmtList(body)
  )
