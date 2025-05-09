open Module_types
module Make (Rb : RULE_BANK) : RULE with module Rb := Rb
