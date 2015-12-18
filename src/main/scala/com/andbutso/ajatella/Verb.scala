package com.andbutso.ajatella

trait Mood
case object Indicative extends Mood
case object Conditional extends Mood
case object Potential extends Mood
case object Imperative extends Mood

trait Tense
case object Infinitive extends Tense
case object Present extends Tense
case object Imperfect extends Tense
case object Perfect extends Tense
case object Pluperfect extends Tense

trait Pronoun // minä, sinä, hän, me, te, he