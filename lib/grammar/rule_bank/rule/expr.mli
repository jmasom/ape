open Module_types
module Make (Rb : RULE_BANK) : EXPR with module Rb = Rb
