module Bubblegum.Vocabulary exposing(..)
{-| This module exposes the rdf vocabulary for Bubblegum.
More about RDF n-triples: https://en.wikipedia.org/wiki/N-Triples

See http://flarebyte.github.io/ontologies/2018/user-interface#
which should be abbreviated as the curie ui:

# RDF vocabulary
@docs  vocabulary

-}
ui_id = "ui:id"
ui_position = "ui:position"
ui_label = "ui:label"
ui_hint = "ui:hint"
ui_prominence = "ui:prominence"
ui_style = "ui:style"
ui_icon = "ui:icon"
ui_validator = "ui:validator" --ex:email
ui_helpValid = "ui:help-valid"
ui_helpInvalid = "ui:help-invalid"
ui_traits = "ui:traits" -- custom traits of the component
ui_regex = "ui:regex"
ui_placeholder = "ui:placeholder"
ui_maxLength = "ui:maximum-length"
ui_minLines = "ui:minimum-lines"
ui_maxLines = "ui:maximum-lines"
ui_languageSyntax = "ui:language-syntax"
ui_settingOfField = "ui:setting-of-field"
ui_partOfPanel = "ui:part-of-panel"
ui_partOfSection = "ui:part-of-section"
ui_partOfDivision = "ui:part-of-division"
ui_mainSelection = "ui:main-selection"
ui_language = "ui:language"
ui_viewMode = "ui:view-mode" --ex: /admin
ui_divisionId = "ui:division-id" --ex: /character
ui_searchTerm = "ui:search-term"
ui_searchFrom = "ui:search-from"
ui_searchTo = "ui:search-to"
ui_searchSelected = "ui:search-selected"
ui_settings = "ui:settings"
ui_settingKey = "ui:setting-key"
ui_settingValues = "ui:setting-values"
ui_settingFacets = "ui:setting-facets" 