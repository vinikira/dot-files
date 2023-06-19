(require 'tempo)

(tempo-define-template "plantuml-mode-c4dynamic"
                       '("@startuml
!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/master/C4_Dynamic.puml

/'
' macros: Person, Person_Ext, System, System_Ext, SystemDb, SystemDb_Ext,
' Boundary, System_Boundary, Enterprise_Boundary, Container, ContainerDb,
' Container_Boundary, Component, ComponentDb, RelIndex, increment, setIndex
'/

Person(personAlias, \"Label\", \"Optional\")
Container(containerAlias, \"Label\", \"Tecnology\", \"Optional description\")
System(systemAlias, \"Label\", \"Optional description\")

Rel(personAlias, containerAlias, \"Label\", \"Optional Tecnology\")
@enduml
")
                       "c4dynamic"
                       "Inserts a c4 dynamic diagram template."
                       'plantuml-tempo-tags)

(tempo-define-template "plantuml-mode-c4component"
                       '("@startuml
!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/master/C4_Component.puml

/'
' macros: Person, Person_Ext, System, System_Ext, SystemDb, SystemDb_Ext,
' Boundary, System_Boundary, Enterprise_Boundary, Container, ContainerDb,
' Container_Boundary, Component, ComponentDb
'/

Person(personAlias, \"Label\", \"Optional\")
Container(containerAlias, \"Label\", \"Tecnology\", \"Optional description\")
System(systemAlias, \"Label\", \"Optional description\")

Rel(personAlias, containerAlias, \"Label\", \"Optional Tecnology\")
@enduml
")
                       "c4component"
                       "Inserts a c4 component diagram template."
                       'plantuml-tempo-tags)

(tempo-define-template "plantuml-mode-c4container"
                       '("@startuml
!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/master/C4_Container.puml

/'
' macros: Person, Person_Ext, System, System_Ext, SystemDb, SystemDb_Ext,
' Boundary, System_Boundary, Enterprise_Boundary, Container, ContainerDb,
' Container_Boundary
'/

!define DEVICONS https://raw.githubusercontent.com/tupadr3/plantuml-icon-font-sprites/master/devicons
!define FONTAWESOME https://raw.githubusercontent.com/tupadr3/plantuml-icon-font-sprites/master/font-awesome-5
!include DEVICONS/angular.puml
!include DEVICONS/java.puml
!include DEVICONS/msql_server.puml
!include FONTAWESOME/users.puml

LAYOUT_WITH_LEGEND()

Person(user, \"Customer\", \"People that need products\", \"users\")
Container(spa, \"SPA\", \"angular\", \"The main interface that the customer interacts with\", \"angular\")
Container(api, \"API\", \"java\", \"Handles all business logic\", \"java\")
ContainerDb(db, \"Database\", \"Microsoft SQL\", \"Holds product, order and invoice information\", \"msql_server\")

Rel(user, spa, \"Uses\", \"https\")
Rel(spa, api, \"Uses\", \"https\")
Rel_R(api, db, \"Reads/Writes\")
@enduml
")
                       "c4container"
                       "Inserts a c4 container diagram template."
                       'plantuml-tempo-tags)

(tempo-define-template "plantuml-mode-c4context"
                       '("@startuml
!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/master/C4_Context.puml

/'
' macros: Person, Person_Ext, System, System_Ext, SystemDb, SystemDb_Ext,
' Boundary, System_Boundary, Enterprise_Boundary
'/

Person(admin, \"Administrator\")

System(web_app, \"WebApp\")

System(twitter, \"Twitter\")

Rel(admin, web_app, \"Uses\", \"HTTPS\")
Rel(web_app, twitter, \"Gets tweets from\", \"HTTPS\")
@enduml")
                       "c4context"
                       "Inserts a c4 context diagram template."
                       'plantuml-tempo-tags)

(tempo-define-template "plantuml-mode-c4deployment"
                       '("@startuml
!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/master/C4_Deployment.puml

/'
' macros: Person, Person_Ext, System, System_Ext, SystemDb, SystemDb_Ext,
' Boundary, System_Boundary, Enterprise_Boundary, Container, ContainerDb,
' Container_Boundary, Component, ComponentDb, RelIndex, increment, setIndex
' Deployment_Node
'/

Person(personAlias, \"Label\", \"Optional\")
Container(containerAlias, \"Label\", \"Tecnology\", \"Optional description\")
System(systemAlias, \"Label\", \"Optional description\")

Rel(personAlias, containerAlias, \"Label\", \"Optional Tecnology\")
@enduml
")
                       "c4deployment"
                       "Inserts a c4 deployment diagram template."
                       'plantuml-tempo-tags)

(tempo-define-template "plantuml-mode-er"
                       '("@startuml
' hide the spot
hide circle

' avoid problems with angled crows feet
skinparam linetype ortho

entity \"Entity01\" as e01 {
  *e1_id : number <<generated>>
  --
  *name : text
  description : text
}

entity \"Entity02\" as e02 {
  *e2_id : number <<generated>>
  --
  *e1_id : number <<FK>>
  other_details : text
}

entity \"Entity03\" as e03 {
  *e3_id : number <<generated>>
  --
  e1_id : number <<FK>>
  other_details : text
}

e01 ||..o{ e02
e01 |o..o{ e03

@enduml")
                       "er"
                       "Inserts a ER diagram template."
                       'plantuml-tempo-tags)

(tempo-define-template
 "plantuml-mode-er-entity"
 '("entity \"" p "\" as " p " {
  *" p " : number <<generated>>
  --
  " p " : number <<FK>>
  " p " : text
}")
 "ent"
 "Inserts a ER entity template."
 'plantuml-tempo-tags)

(tempo-define-template "plantuml-mode-monochrome"
                       '("skinparam monochrome true
skinparam defaultFontName Iosevka Nerd Font
skinparam ranksep 20
skinparam dpi 150
skinparam arrowThickness 0.7
skinparam packageTitleAlignment left
skinparam usecaseBorderThickness 0.4
skinparam defaultFontSize 12
skinparam rectangleBorderThickness 1
")
                       "monochrome"
                       "Inserts monochrome setup."
                       'plantuml-tempo-tags)


(tempo-define-template
 "plantuml-mode-note"
 '("note " p " of " p n p n "end note")
 "n"
 "Inserts note template."
 'plantuml-tempo-tags)
