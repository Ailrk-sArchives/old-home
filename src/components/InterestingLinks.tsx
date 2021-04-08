import {Container} from 'react-bootstrap';
import {HoverLink} from './Misc';
import {FaLink} from "react-icons/fa/index";
import React from 'react';
import "./About.css"; // reuse about's style
import {useWindowSize} from '../state/hooks';

type Genre =
  | "website"
  | "tool"
  | "library"
  | "paper"
  | "game"
  | "talk"
  | "book"
  | "anime"
  | "music"
  | "blog"
  | "noice"
  ;

const links: Array<[Genre, string, string]> = [

  ["noice", "vibin'", "https://www.youtube.com/watch?v=qMQ-y9dHE2k"],

  ["paper", "The connection between c++ template metaprogramming and functional programming",
    "http://www.tnkcs.inf.elte.hu/vedes/sinkovics_abel_ertekezes.pdf"],

  ["tool", "Try APL", "https://tryapl.org/"],

  ["paper", "Applicative Functor with Effects", "http://www.staff.city.ac.uk/~ross/papers/Applicative.pdf"],

  ["website", "CMSC 430: Design and Implementation of Programming Languages", "https://www.cs.umd.edu/class/spring2020/cmsc430/index.html"],

  ["blog", "LL and LR Parsing Demystified", "https://blog.reverberate.org/2013/07/ll-and-lr-parsing-demystified.html"],

  ["tool", "Compiler Explorer (C++)", "https://godbolt.org/"],

  ["paper", "No-Brainer CPS Conversion", "https://www.ccs.neu.edu/home/shivers/papers/nobrainer-cps.pdf"],

  ["library", "parsec: Monadic parser combinators", "https://hackage.haskell.org/package/parsec-3.1.14.0"],

  ["book", "Functional programming in OCaml", "https://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/"],

  ["blog", "Memory Ordering at Compile Time", "https://preshing.com/20120625/memory-ordering-at-compile-time/"],

  ["paper", "Linear Haskell", "https://arxiv.org/pdf/1710.09756.pdf"],

  ["book", "Practical Common Lisp", "http://www.gigamonkeys.com/book/"],

  ["book", "Left Over Lambda", "https://letoverlambda.com/index.cl/toc"],

  ["website", "Error-Correcting Codes, Finite Fields, ...", "https://www-users.math.umn.edu/~garrett/coding/Overheads/"],

  ["book", "A History of Haskell: Being Lazy With CLass", "https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf?from=https%3A%2F%2Fresearch.microsoft.com%2Fen-us%2Fum%2Fpeople%2Fsimonpj%2Fpapers%2Fhistory-of-haskell%2Fhistory.pdf"],

  ["book", "A History of C++: 1979-1991", "https://dl.acm.org/doi/pdf/10.1145/234286.1057836"],

  ["game", "superliminal", "https://store.steampowered.com/app/1049410/_Superliminal/"],

  ["talk", "CppCon 2017: Fedor Pikus C++ atomics, from basic to advanced. What do they really do?", "https://www.youtube.com/watch?v=ZQFzMfHIxng&t=603s"],


  ["talk", "CppCon 2014: Chandler Carruth Efficiency with Algorithms, Performance with Data Structures", "https://www.youtube.com/watch?v=fHNmRkzxHWs&t=2997s"],

  ["blog", "Arrows: A General Interface to Computation", "https://www.haskell.org/arrows/"],

  ["paper", "Reactor", "https://www.dre.vanderbilt.edu/~schmidt/PDF/reactor-siemens.pdf"],

  ["website", "IO Wiki", "https://oi-wiki.org/"],

  ["book", "From Mathematics To Generic Programming", "https://www.fm2gp.com/"],

  ["paper", "Type classes in Haskell", "https://link.springer.com/content/pdf/10.1007%2F3-540-57880-3_16.pdf"],

  ["paper", "Parsing technique: A practical guide", "https://doc.lagout.org/science/0_Computer%20Science/4_Theory%20of%20Computation/Compiler%20Design/Parsing%20Techniques%20-%20A%20Practical%20Guide%2C%202nd%20Edition.pdf"],

  ["book", "Modern Compiler Implementation in ML", "https://www.cs.princeton.edu/~appel/modern/ml/"],

  ["paper", "Continuation-Passing, Closure-Passing Style", "https://www.classes.cs.uchicago.edu/archive/2006/spring/32630-1/papers/p293-appel.pdf"],

  ["paper", "Promoting Functions to Type Families in Haskell", "https://repository.brynmawr.edu/cgi/viewcontent.cgi?article=1000&context=compsci_pubs"],

  ["website", "My First Language Frontend with LLVM Tutorial", "https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html"],

  ["paper", "Lectures on the Curry-Howard Isomorphism", "https://disi.unitn.it/~bernardi/RSISE11/Papers/curry-howard.pdf"],

  ["website", "Why Optional Referencess Didn't make it In C++ 17", "https://www.fluentcpp.com/2018/10/05/pros-cons-optional-references/"],

  ["paper", "A Fast and Elistist Multiobjective Genetic Algorihtm: NSGA-II", "https://www.iitk.ac.in/kangal/Deb_NSGA-II.pdf"],

  ["book", "sicp", "https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html"],

  ["paper", "Essence of generalized partial computation", "https://pdf.sciencedirectassets.com/271538/1-s2.0-S0304397500X02575/1-s2.0-030439759190299H/main.pdf?X-Amz-Security-Token=IQoJb3JpZ2luX2VjEP7%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJHMEUCIQD9FytqkZbu2Jn5tZ98Yu28bcwwFhE6%2FZF5Gv0sWmb2nAIgcqOUDishkj%2Fbr84DS5E9CKo6XrsoGp%2B6ZcS4Hee2XzsqtAMIVxADGgwwNTkwMDM1NDY4NjUiDDSX5dsh5vkGyxvIJCqRA7XI6BBVsOC6Y5rHOqjHYO7%2Fk9zwYC73drCCpeOxM3phl5kqeWdjyHH8BEbeC7RMk3XgE2%2Fpqs9UxS7uLuAPM7TK%2BCpDa05SDK1qndo%2F6h%2FaK%2B1%2FxXIaLyc73tjk%2FpxA%2F%2B2GnGttdiaAVzn3QbcliJdocAQwOltVtWf2X5Oq8D99LwOrWXm9qdjZ4zKOp9GSrHxIbg0EIbis7VHyzorplQydzNp9ug6CyZmMJAUZ8Nxe1KkpXiSfpp9wvDU5W%2BPfE7etzukaicq%2FKAhirYm%2F0URFtMJlm8vqDTjre0h98xX3rLUxr12rGF%2FUxdd2qYmoPi6ojd93vWzie%2B4KGNyMuQgjIU0Nxhu6S4GnZgILoW1YlPTuESQiFY3EOEREuM8%2Bdwj4RTgkcMJP%2F5O%2FaTrIbb8VLJGSioHiVNwe20iCGF%2FD0Rq1RVChXhbJ9S1jzODAeXHzwySRRzSwoNpfCzbPtVKXk6jBexnwHboqRGMN%2BJBtarX2mJw8IjSOLz%2FKKTUwbAtzyj5U%2FfqeBHYe9GLu0Xy0MMWyuoMGOusBp41iCTQTA%2FuRn0xGIEUpDkJTaOljoZXlJYKV51Unn28OhzG604zTvsUWo9nIt5dq6BPAwZbuXr9MiqfRETkTyDcXC5vz9aJUzNfJZGSfTQqWsYJmWKMOB%2B%2B8TFd0cP2n6w6AhCyc%2FZkIhkpg0Tkz1KqY%2BzeBNZOu1bKX47ZAjhZFfttbYGCogmDl%2BjUBT5LOB1uKUgb4xWh4EbZpX87di3eqHncQjziTT99aH1TREeY5iFlfIXa%2FM1xcdfdoRV8JPv6jVPTyw3X4jVD20Ob5twexNB5uEA8BqHHVdx0gWTp0QBoAttGgKnBJtA%3D%3D&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20210408T063226Z&X-Amz-SignedHeaders=host&X-Amz-Expires=300&X-Amz-Credential=ASIAQ3PHCVTYVZK2BRHP%2F20210408%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=81efa56669cb4a28e820a6040523747aa9dd0cab1c5037aa6cbc6fd5fd4b5ee1&hash=d514dfab03de10b0a217eaad58961c53919207c155d42da488eac39918520dc0&host=68042c943591013ac2b2430a89b270f6af2c76d8dfd086a07176afe7c76c2c61&pii=030439759190299H&tid=spdf-e90560a1-8f81-49f2-bebc-2baf73f1aa81&sid=45a92b6e7b1b204adf8bfd55155715e7c4d0gxrqa&type=client"],

  ["book", "Learn Vim the hard way", "https://learnvimscriptthehardway.stevelosh.com/"],

  ["blog", "A deep dive into React Fiber Internals", "https://blog.logrocket.com/deep-dive-into-react-fiber-internals/"],

  ["paper", "The Essence of Event-Driven Programming", "https://jpaykin.github.io/papers/pkz_CONCUR_2016.pdf"],

  ["paper", "The Seasoned Schemer", "https://doc.lagout.org/programmation/Lisp/Scheme/The%20Seasoned%20Schemer%20-%20Daniel%20P.%20Friedman.pdf"],

  ["paper", "The Little Schemer", "https://mitpress.mit.edu/books/little-schemer-fourth-edition"],

  ["paper", "Embedding effect system in Haskell", "https://www.doc.ic.ac.uk/~dorchard/publ/haskell14-effects.pdf"],

  ["paper", "Higher Order Abstract Syntax", "https://www.cs.cmu.edu/~fp/papers/pldi88.pdf"],

  ["blog", "How to compile with continuations", "http://matt.might.net/articles/cps-conversion/"],

  ["blog", "A-Normalizaiton: Why and How", "http://matt.might.net/articles/a-normalization/"],

  ["paper", "AddressSanitizer: A Fast Address Sanity Checker", "https://static.googleusercontent.com/media/research.google.com/zh-CN//pubs/archive/37752.pdf"],

  ["blog", "By example: Continuation-passing style in Javascript", "http://matt.might.net/articles/by-example-continuation-passing-style/"],

  ["book", "The Garbage Collection Handbook", "https://gchandbook.org/"],
  ["music", "Larry Carlton - Room 335", "https://www.youtube.com/watch?v=47ysdThtXgw"],

  ["paper", "How to CPS Transform a Monad", "https://link.springer.com/content/pdf/10.1007%2F978-3-642-00722-4_19.pdf"],

  ["paper", "Profunctor Optics: Modular Data Accessors", "https://arxiv.org/pdf/1703.10857.pdf"],

  ["paper", "Revised Report on the Algorithmic Language ALGOL 60", "http://standardpascaline.org/Algol60-RevisedReport.pdf"],

  ["paper", "Control Flow Analysis", "https://www.cs.columbia.edu/~suman/secure_sw_devel/p1-allen.pdf"],

  ["book", "Advanced Compiler Design and Implementation", "https://www.amazon.ca/Advanced-Compiler-Design-Implementation-Muchnick/dp/1558603204"]
];

export function InterestingLinks() {
  const {width} = useWindowSize();
  return (
    <Container style={width > 600 ? {} : {marginLeft: 30}}>
      <div>
        <h5>Interesting Links</h5>
      </div>
      {
        links.map(pair => {
          const [genre, name, link] = pair;

          return (
            <HoverLink text={` [${genre}] ` + name}
              link={link}
              ogColor={"black"}
              onHoverColor={"LightCoral"}
              element={e => (<h5 className="about-info"> <FaLink />{e}</h5>)} />);
        })
      }
    </Container>);
}
