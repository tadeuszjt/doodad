module grep

import ../std/io
import ../std/strings
import ../std/chars


type CharacterClass {
    CharacterClassAnyWord() |
    CharacterClassAnyWordInverted() |
    CharacterClassAnyDecimalDigit() |
    CharacterClassAnyDecimalDigitInverted()
}

fn string(c CharacterClass) string
    switch c
        CharacterClassAnyWord(); return "\\w"
        CharacterClassAnyWordInverted(); return "\\W"
        CharacterClassAnyDecimalDigit(); return "\\d"
        CharacterClassAnyDecimalDigitInverted(); return "\\D"


type CharacterClassFromUnicodeCategory string

fn string(c CharacterClassFromUnicodeCategory) string
    return "\\p{" + string(c) + "}"


type CharacterRange (char, { null | char })

fn string(cr CharacterRange) string
    switch cr
        (c, null);     return [c]
        (c, char(c2)); return [c, '-', c2]

type CharacterGroupItem {
    CharacterClass |
    CharacterClassFromUnicodeCategory |
    CharacterRange |
    char
}

type CharacterGroupNegativeModifier bool
type CharacterGroup (CharacterGroupNegativeModifier, [CharacterGroupItem] )


type MatchCharacter char
type MatchCharacterClass { CharacterGroup }
type MatchItem { MatchAnyCharacter() | MatchCharacter }



fn main()
    let c = CharacterClassAnyDecimalDigitInverted()
    print(c)
    let x = CharacterRange('a', {'b'})
    print (x)

    print("regex")


