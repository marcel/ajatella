package com.andbutso.ajatella

class Lemmatizer {

}

// cf. http://snowball.tartarus.org/algorithms/finnish/stemmer.html)
//
//class FinnishLightStemmer {
//
//  public int stem(char s[], int len) {
//    if (len < 4)
//      return len;
//
//    for (int i = 0; i < len; i++)
//    switch(s[i]) {
//      case 'ä':
//      case 'å': s[i] = 'a'; break;
//      case 'ö': s[i] = 'o'; break;
//    }
//
//    len = step1(s, len);
//    len = step2(s, len);
//    len = step3(s, len);
//    len = norm1(s, len);
//    len = norm2(s, len);
//    return len;
//  }
//
//  private int step1(char s[], int len) {
//    if (len > 8) {
//      if (endsWith(s, len, "kin"))
//        return step1(s, len-3);
//      if (endsWith(s, len, "ko"))
//        return step1(s, len-2);
//    }
//
//    if (len > 11) {
//      if (endsWith(s, len, "dellinen"))
//        return len-8;
//      if (endsWith(s, len, "dellisuus"))
//        return len-9;
//    }
//    return len;
//  }
//
//  private int step2(char s[], int len) {
//    if (len > 5) {
//      if (endsWith(s, len, "lla")
//        || endsWith(s, len, "tse")
//        || endsWith(s, len, "sti"))
//        return len-3;
//
//      if (endsWith(s, len, "ni"))
//        return len-2;
//
//      if (endsWith(s, len, "aa"))
//        return len-1; // aa -> a
//    }
//
//    return len;
//  }
//
//  private int step3(char s[], int len) {
//    if (len > 8) {
//      if (endsWith(s, len, "nnen")) {
//        s[len-4] = 's';
//        return len-3;
//      }
//
//      if (endsWith(s, len, "ntena")) {
//        s[len-5] = 's';
//        return len-4;
//      }
//
//      if (endsWith(s, len, "tten"))
//        return len-4;
//
//      if (endsWith(s, len, "eiden"))
//        return len-5;
//    }
//
//    if (len > 6) {
//      if (endsWith(s, len, "neen")
//        || endsWith(s, len, "niin")
//        || endsWith(s, len, "seen")
//        || endsWith(s, len, "teen")
//        || endsWith(s, len, "inen"))
//        return len-4;
//
//      if (s[len-3] == 'h' && isVowel(s[len-2]) && s[len-1] == 'n')
//      return len-3;
//
//      if (endsWith(s, len, "den")) {
//        s[len-3] = 's';
//        return len-2;
//      }
//
//      if (endsWith(s, len, "ksen")) {
//        s[len-4] = 's';
//        return len-3;
//      }
//
//      if (endsWith(s, len, "ssa")
//        || endsWith(s, len, "sta")
//        || endsWith(s, len, "lla")
//        || endsWith(s, len, "lta")
//        || endsWith(s, len, "tta")
//        || endsWith(s, len, "ksi")
//        || endsWith(s, len, "lle"))
//        return len-3;
//    }
//
//    if (len > 5) {
//      if (endsWith(s, len, "na")
//        || endsWith(s, len, "ne"))
//        return len-2;
//
//      if (endsWith(s, len, "nei"))
//        return len-3;
//    }
//
//    if (len > 4) {
//      if (endsWith(s, len, "ja")
//        || endsWith(s, len, "ta"))
//        return len-2;
//
//      if (s[len-1] == 'a')
//      return len-1;
//
//      if (s[len-1] == 'n' && isVowel(s[len-2]))
//      return len-2;
//
//      if (s[len-1] == 'n')
//      return len-1;
//    }
//
//    return len;
//  }
//
//  private int norm1(char s[], int len) {
//    if (len > 5 && endsWith(s, len, "hde")) {
//      s[len-3] = 'k';
//      s[len-2] = 's';
//      s[len-1] = 'i';
//    }
//
//    if (len > 4) {
//      if (endsWith(s, len, "ei") || endsWith(s, len, "at"))
//        return len-2;
//    }
//
//    if (len > 3)
//      switch(s[len-1]) {
//      case 't':
//      case 's':
//      case 'j':
//      case 'e':
//      case 'a':
//      case 'i': return len-1;
//    }
//
//    return len;
//  }
//
//  private int norm2(char s[], int len) {
//    if (len > 8) {
//      if (s[len-1] == 'e'
//      || s[len-1] == 'o'
//      || s[len-1] == 'u')
//      len--;
//    }
//
//    if (len > 4) {
//      if (s[len-1] == 'i')
//      len--;
//
//      if (len > 4) {
//        char ch = s[0];
//        for (int i = 1; i < len; i++) {
//          if (s[i] == ch &&
//            (ch == 'k' || ch == 'p' || ch == 't'))
//            len = delete(s, i--, len);
//          else
//            ch = s[i];
//        }
//      }
//    }
//
//    return len;
//  }
//
//  private boolean isVowel(char ch) {
//    switch(ch) {
//      case 'a':
//      case 'e':
//      case 'i':
//      case 'o':
//      case 'u':
//      case 'y': return true;
//      default: return false;
//    }
//  }
//}