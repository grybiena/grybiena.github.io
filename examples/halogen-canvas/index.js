(() => {
  // output/Control.Bind/foreign.js
  var arrayBind = function(arr) {
    return function(f) {
      var result = [];
      for (var i2 = 0, l = arr.length; i2 < l; i2++) {
        Array.prototype.push.apply(result, f(arr[i2]));
      }
      return result;
    };
  };

  // output/Control.Apply/foreign.js
  var arrayApply = function(fs) {
    return function(xs) {
      var l = fs.length;
      var k = xs.length;
      var result = new Array(l * k);
      var n = 0;
      for (var i2 = 0; i2 < l; i2++) {
        var f = fs[i2];
        for (var j = 0; j < k; j++) {
          result[n++] = f(xs[j]);
        }
      }
      return result;
    };
  };

  // output/Control.Semigroupoid/index.js
  var semigroupoidFn = {
    compose: function(f) {
      return function(g) {
        return function(x) {
          return f(g(x));
        };
      };
    }
  };

  // output/Control.Category/index.js
  var identity = function(dict) {
    return dict.identity;
  };
  var categoryFn = {
    identity: function(x) {
      return x;
    },
    Semigroupoid0: function() {
      return semigroupoidFn;
    }
  };

  // output/Data.Boolean/index.js
  var otherwise = true;

  // output/Data.Function/index.js
  var flip = function(f) {
    return function(b2) {
      return function(a2) {
        return f(a2)(b2);
      };
    };
  };
  var $$const = function(a2) {
    return function(v) {
      return a2;
    };
  };
  var applyFlipped = function(x) {
    return function(f) {
      return f(x);
    };
  };

  // output/Data.Functor/foreign.js
  var arrayMap = function(f) {
    return function(arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i2 = 0; i2 < l; i2++) {
        result[i2] = f(arr[i2]);
      }
      return result;
    };
  };

  // output/Data.Unit/foreign.js
  var unit = void 0;

  // output/Type.Proxy/index.js
  var $$Proxy = /* @__PURE__ */ function() {
    function $$Proxy2() {
    }
    ;
    $$Proxy2.value = new $$Proxy2();
    return $$Proxy2;
  }();

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };
  var voidLeft = function(dictFunctor) {
    var map112 = map(dictFunctor);
    return function(f) {
      return function(x) {
        return map112($$const(x))(f);
      };
    };
  };
  var voidRight = function(dictFunctor) {
    var map112 = map(dictFunctor);
    return function(x) {
      return map112($$const(x));
    };
  };
  var functorArray = {
    map: arrayMap
  };

  // output/Control.Apply/index.js
  var identity2 = /* @__PURE__ */ identity(categoryFn);
  var applyArray = {
    apply: arrayApply,
    Functor0: function() {
      return functorArray;
    }
  };
  var apply = function(dict) {
    return dict.apply;
  };
  var applySecond = function(dictApply) {
    var apply1 = apply(dictApply);
    var map28 = map(dictApply.Functor0());
    return function(a2) {
      return function(b2) {
        return apply1(map28($$const(identity2))(a2))(b2);
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var unless = function(dictApplicative) {
    var pure13 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (!v) {
          return v1;
        }
        ;
        if (v) {
          return pure13(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 68, column 1 - line 68, column 65): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var when = function(dictApplicative) {
    var pure13 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (v) {
          return v1;
        }
        ;
        if (!v) {
          return pure13(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var liftA1 = function(dictApplicative) {
    var apply3 = apply(dictApplicative.Apply0());
    var pure13 = pure(dictApplicative);
    return function(f) {
      return function(a2) {
        return apply3(pure13(f))(a2);
      };
    };
  };

  // output/Control.Bind/index.js
  var discard = function(dict) {
    return dict.discard;
  };
  var bindArray = {
    bind: arrayBind,
    Apply0: function() {
      return applyArray;
    }
  };
  var bind = function(dict) {
    return dict.bind;
  };
  var bindFlipped = function(dictBind) {
    return flip(bind(dictBind));
  };
  var composeKleisliFlipped = function(dictBind) {
    var bindFlipped12 = bindFlipped(dictBind);
    return function(f) {
      return function(g) {
        return function(a2) {
          return bindFlipped12(f)(g(a2));
        };
      };
    };
  };
  var discardUnit = {
    discard: function(dictBind) {
      return bind(dictBind);
    }
  };

  // output/Effect.Aff/foreign.js
  var Aff = function() {
    var EMPTY = {};
    var PURE = "Pure";
    var THROW = "Throw";
    var CATCH = "Catch";
    var SYNC = "Sync";
    var ASYNC = "Async";
    var BIND = "Bind";
    var BRACKET = "Bracket";
    var FORK = "Fork";
    var SEQ = "Sequential";
    var MAP = "Map";
    var APPLY = "Apply";
    var ALT = "Alt";
    var CONS = "Cons";
    var RESUME = "Resume";
    var RELEASE = "Release";
    var FINALIZER = "Finalizer";
    var FINALIZED = "Finalized";
    var FORKED = "Forked";
    var FIBER = "Fiber";
    var THUNK = "Thunk";
    function Aff2(tag, _1, _2, _3) {
      this.tag = tag;
      this._1 = _1;
      this._2 = _2;
      this._3 = _3;
    }
    function AffCtr(tag) {
      var fn = function(_1, _2, _3) {
        return new Aff2(tag, _1, _2, _3);
      };
      fn.tag = tag;
      return fn;
    }
    function nonCanceler2(error4) {
      return new Aff2(PURE, void 0);
    }
    function runEff(eff) {
      try {
        eff();
      } catch (error4) {
        setTimeout(function() {
          throw error4;
        }, 0);
      }
    }
    function runSync(left, right, eff) {
      try {
        return right(eff());
      } catch (error4) {
        return left(error4);
      }
    }
    function runAsync(left, eff, k) {
      try {
        return eff(k)();
      } catch (error4) {
        k(left(error4))();
        return nonCanceler2;
      }
    }
    var Scheduler = function() {
      var limit = 1024;
      var size4 = 0;
      var ix = 0;
      var queue = new Array(limit);
      var draining = false;
      function drain() {
        var thunk;
        draining = true;
        while (size4 !== 0) {
          size4--;
          thunk = queue[ix];
          queue[ix] = void 0;
          ix = (ix + 1) % limit;
          thunk();
        }
        draining = false;
      }
      return {
        isDraining: function() {
          return draining;
        },
        enqueue: function(cb) {
          var i2, tmp;
          if (size4 === limit) {
            tmp = draining;
            drain();
            draining = tmp;
          }
          queue[(ix + size4) % limit] = cb;
          size4++;
          if (!draining) {
            drain();
          }
        }
      };
    }();
    function Supervisor(util) {
      var fibers = {};
      var fiberId = 0;
      var count = 0;
      return {
        register: function(fiber) {
          var fid = fiberId++;
          fiber.onComplete({
            rethrow: true,
            handler: function(result) {
              return function() {
                count--;
                delete fibers[fid];
              };
            }
          })();
          fibers[fid] = fiber;
          count++;
        },
        isEmpty: function() {
          return count === 0;
        },
        killAll: function(killError, cb) {
          return function() {
            if (count === 0) {
              return cb();
            }
            var killCount = 0;
            var kills = {};
            function kill2(fid) {
              kills[fid] = fibers[fid].kill(killError, function(result) {
                return function() {
                  delete kills[fid];
                  killCount--;
                  if (util.isLeft(result) && util.fromLeft(result)) {
                    setTimeout(function() {
                      throw util.fromLeft(result);
                    }, 0);
                  }
                  if (killCount === 0) {
                    cb();
                  }
                };
              })();
            }
            for (var k in fibers) {
              if (fibers.hasOwnProperty(k)) {
                killCount++;
                kill2(k);
              }
            }
            fibers = {};
            fiberId = 0;
            count = 0;
            return function(error4) {
              return new Aff2(SYNC, function() {
                for (var k2 in kills) {
                  if (kills.hasOwnProperty(k2)) {
                    kills[k2]();
                  }
                }
              });
            };
          };
        }
      };
    }
    var SUSPENDED = 0;
    var CONTINUE = 1;
    var STEP_BIND = 2;
    var STEP_RESULT = 3;
    var PENDING = 4;
    var RETURN = 5;
    var COMPLETED = 6;
    function Fiber(util, supervisor, aff) {
      var runTick = 0;
      var status = SUSPENDED;
      var step3 = aff;
      var fail2 = null;
      var interrupt = null;
      var bhead = null;
      var btail = null;
      var attempts = null;
      var bracketCount = 0;
      var joinId = 0;
      var joins = null;
      var rethrow = true;
      function run3(localRunTick) {
        var tmp, result, attempt;
        while (true) {
          tmp = null;
          result = null;
          attempt = null;
          switch (status) {
            case STEP_BIND:
              status = CONTINUE;
              try {
                step3 = bhead(step3);
                if (btail === null) {
                  bhead = null;
                } else {
                  bhead = btail._1;
                  btail = btail._2;
                }
              } catch (e) {
                status = RETURN;
                fail2 = util.left(e);
                step3 = null;
              }
              break;
            case STEP_RESULT:
              if (util.isLeft(step3)) {
                status = RETURN;
                fail2 = step3;
                step3 = null;
              } else if (bhead === null) {
                status = RETURN;
              } else {
                status = STEP_BIND;
                step3 = util.fromRight(step3);
              }
              break;
            case CONTINUE:
              switch (step3.tag) {
                case BIND:
                  if (bhead) {
                    btail = new Aff2(CONS, bhead, btail);
                  }
                  bhead = step3._2;
                  status = CONTINUE;
                  step3 = step3._1;
                  break;
                case PURE:
                  if (bhead === null) {
                    status = RETURN;
                    step3 = util.right(step3._1);
                  } else {
                    status = STEP_BIND;
                    step3 = step3._1;
                  }
                  break;
                case SYNC:
                  status = STEP_RESULT;
                  step3 = runSync(util.left, util.right, step3._1);
                  break;
                case ASYNC:
                  status = PENDING;
                  step3 = runAsync(util.left, step3._1, function(result2) {
                    return function() {
                      if (runTick !== localRunTick) {
                        return;
                      }
                      runTick++;
                      Scheduler.enqueue(function() {
                        if (runTick !== localRunTick + 1) {
                          return;
                        }
                        status = STEP_RESULT;
                        step3 = result2;
                        run3(runTick);
                      });
                    };
                  });
                  return;
                case THROW:
                  status = RETURN;
                  fail2 = util.left(step3._1);
                  step3 = null;
                  break;
                // Enqueue the Catch so that we can call the error handler later on
                // in case of an exception.
                case CATCH:
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step3, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step3, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step3 = step3._1;
                  break;
                // Enqueue the Bracket so that we can call the appropriate handlers
                // after resource acquisition.
                case BRACKET:
                  bracketCount++;
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step3, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step3, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step3 = step3._1;
                  break;
                case FORK:
                  status = STEP_RESULT;
                  tmp = Fiber(util, supervisor, step3._2);
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
                  if (step3._1) {
                    tmp.run();
                  }
                  step3 = util.right(tmp);
                  break;
                case SEQ:
                  status = CONTINUE;
                  step3 = sequential3(util, supervisor, step3._1);
                  break;
              }
              break;
            case RETURN:
              bhead = null;
              btail = null;
              if (attempts === null) {
                status = COMPLETED;
                step3 = interrupt || fail2 || step3;
              } else {
                tmp = attempts._3;
                attempt = attempts._1;
                attempts = attempts._2;
                switch (attempt.tag) {
                  // We cannot recover from an unmasked interrupt. Otherwise we should
                  // continue stepping, or run the exception handler if an exception
                  // was raised.
                  case CATCH:
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      status = RETURN;
                    } else if (fail2) {
                      status = CONTINUE;
                      step3 = attempt._2(util.fromLeft(fail2));
                      fail2 = null;
                    }
                    break;
                  // We cannot resume from an unmasked interrupt or exception.
                  case RESUME:
                    if (interrupt && interrupt !== tmp && bracketCount === 0 || fail2) {
                      status = RETURN;
                    } else {
                      bhead = attempt._1;
                      btail = attempt._2;
                      status = STEP_BIND;
                      step3 = util.fromRight(step3);
                    }
                    break;
                  // If we have a bracket, we should enqueue the handlers,
                  // and continue with the success branch only if the fiber has
                  // not been interrupted. If the bracket acquisition failed, we
                  // should not run either.
                  case BRACKET:
                    bracketCount--;
                    if (fail2 === null) {
                      result = util.fromRight(step3);
                      attempts = new Aff2(CONS, new Aff2(RELEASE, attempt._2, result), attempts, tmp);
                      if (interrupt === tmp || bracketCount > 0) {
                        status = CONTINUE;
                        step3 = attempt._3(result);
                      }
                    }
                    break;
                  // Enqueue the appropriate handler. We increase the bracket count
                  // because it should not be cancelled.
                  case RELEASE:
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step3, fail2), attempts, interrupt);
                    status = CONTINUE;
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      step3 = attempt._1.killed(util.fromLeft(interrupt))(attempt._2);
                    } else if (fail2) {
                      step3 = attempt._1.failed(util.fromLeft(fail2))(attempt._2);
                    } else {
                      step3 = attempt._1.completed(util.fromRight(step3))(attempt._2);
                    }
                    fail2 = null;
                    bracketCount++;
                    break;
                  case FINALIZER:
                    bracketCount++;
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step3, fail2), attempts, interrupt);
                    status = CONTINUE;
                    step3 = attempt._1;
                    break;
                  case FINALIZED:
                    bracketCount--;
                    status = RETURN;
                    step3 = attempt._1;
                    fail2 = attempt._2;
                    break;
                }
              }
              break;
            case COMPLETED:
              for (var k in joins) {
                if (joins.hasOwnProperty(k)) {
                  rethrow = rethrow && joins[k].rethrow;
                  runEff(joins[k].handler(step3));
                }
              }
              joins = null;
              if (interrupt && fail2) {
                setTimeout(function() {
                  throw util.fromLeft(fail2);
                }, 0);
              } else if (util.isLeft(step3) && rethrow) {
                setTimeout(function() {
                  if (rethrow) {
                    throw util.fromLeft(step3);
                  }
                }, 0);
              }
              return;
            case SUSPENDED:
              status = CONTINUE;
              break;
            case PENDING:
              return;
          }
        }
      }
      function onComplete(join4) {
        return function() {
          if (status === COMPLETED) {
            rethrow = rethrow && join4.rethrow;
            join4.handler(step3)();
            return function() {
            };
          }
          var jid = joinId++;
          joins = joins || {};
          joins[jid] = join4;
          return function() {
            if (joins !== null) {
              delete joins[jid];
            }
          };
        };
      }
      function kill2(error4, cb) {
        return function() {
          if (status === COMPLETED) {
            cb(util.right(void 0))();
            return function() {
            };
          }
          var canceler = onComplete({
            rethrow: false,
            handler: function() {
              return cb(util.right(void 0));
            }
          })();
          switch (status) {
            case SUSPENDED:
              interrupt = util.left(error4);
              status = COMPLETED;
              step3 = interrupt;
              run3(runTick);
              break;
            case PENDING:
              if (interrupt === null) {
                interrupt = util.left(error4);
              }
              if (bracketCount === 0) {
                if (status === PENDING) {
                  attempts = new Aff2(CONS, new Aff2(FINALIZER, step3(error4)), attempts, interrupt);
                }
                status = RETURN;
                step3 = null;
                fail2 = null;
                run3(++runTick);
              }
              break;
            default:
              if (interrupt === null) {
                interrupt = util.left(error4);
              }
              if (bracketCount === 0) {
                status = RETURN;
                step3 = null;
                fail2 = null;
              }
          }
          return canceler;
        };
      }
      function join3(cb) {
        return function() {
          var canceler = onComplete({
            rethrow: false,
            handler: cb
          })();
          if (status === SUSPENDED) {
            run3(runTick);
          }
          return canceler;
        };
      }
      return {
        kill: kill2,
        join: join3,
        onComplete,
        isSuspended: function() {
          return status === SUSPENDED;
        },
        run: function() {
          if (status === SUSPENDED) {
            if (!Scheduler.isDraining()) {
              Scheduler.enqueue(function() {
                run3(runTick);
              });
            } else {
              run3(runTick);
            }
          }
        }
      };
    }
    function runPar(util, supervisor, par, cb) {
      var fiberId = 0;
      var fibers = {};
      var killId = 0;
      var kills = {};
      var early = new Error("[ParAff] Early exit");
      var interrupt = null;
      var root = EMPTY;
      function kill2(error4, par2, cb2) {
        var step3 = par2;
        var head3 = null;
        var tail2 = null;
        var count = 0;
        var kills2 = {};
        var tmp, kid;
        loop: while (true) {
          tmp = null;
          switch (step3.tag) {
            case FORKED:
              if (step3._3 === EMPTY) {
                tmp = fibers[step3._1];
                kills2[count++] = tmp.kill(error4, function(result) {
                  return function() {
                    count--;
                    if (count === 0) {
                      cb2(result)();
                    }
                  };
                });
              }
              if (head3 === null) {
                break loop;
              }
              step3 = head3._2;
              if (tail2 === null) {
                head3 = null;
              } else {
                head3 = tail2._1;
                tail2 = tail2._2;
              }
              break;
            case MAP:
              step3 = step3._2;
              break;
            case APPLY:
            case ALT:
              if (head3) {
                tail2 = new Aff2(CONS, head3, tail2);
              }
              head3 = step3;
              step3 = step3._1;
              break;
          }
        }
        if (count === 0) {
          cb2(util.right(void 0))();
        } else {
          kid = 0;
          tmp = count;
          for (; kid < tmp; kid++) {
            kills2[kid] = kills2[kid]();
          }
        }
        return kills2;
      }
      function join3(result, head3, tail2) {
        var fail2, step3, lhs, rhs, tmp, kid;
        if (util.isLeft(result)) {
          fail2 = result;
          step3 = null;
        } else {
          step3 = result;
          fail2 = null;
        }
        loop: while (true) {
          lhs = null;
          rhs = null;
          tmp = null;
          kid = null;
          if (interrupt !== null) {
            return;
          }
          if (head3 === null) {
            cb(fail2 || step3)();
            return;
          }
          if (head3._3 !== EMPTY) {
            return;
          }
          switch (head3.tag) {
            case MAP:
              if (fail2 === null) {
                head3._3 = util.right(head3._1(util.fromRight(step3)));
                step3 = head3._3;
              } else {
                head3._3 = fail2;
              }
              break;
            case APPLY:
              lhs = head3._1._3;
              rhs = head3._2._3;
              if (fail2) {
                head3._3 = fail2;
                tmp = true;
                kid = killId++;
                kills[kid] = kill2(early, fail2 === lhs ? head3._2 : head3._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail2 === null) {
                      join3(fail2, null, null);
                    } else {
                      join3(fail2, tail2._1, tail2._2);
                    }
                  };
                });
                if (tmp) {
                  tmp = false;
                  return;
                }
              } else if (lhs === EMPTY || rhs === EMPTY) {
                return;
              } else {
                step3 = util.right(util.fromRight(lhs)(util.fromRight(rhs)));
                head3._3 = step3;
              }
              break;
            case ALT:
              lhs = head3._1._3;
              rhs = head3._2._3;
              if (lhs === EMPTY && util.isLeft(rhs) || rhs === EMPTY && util.isLeft(lhs)) {
                return;
              }
              if (lhs !== EMPTY && util.isLeft(lhs) && rhs !== EMPTY && util.isLeft(rhs)) {
                fail2 = step3 === lhs ? rhs : lhs;
                step3 = null;
                head3._3 = fail2;
              } else {
                head3._3 = step3;
                tmp = true;
                kid = killId++;
                kills[kid] = kill2(early, step3 === lhs ? head3._2 : head3._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail2 === null) {
                      join3(step3, null, null);
                    } else {
                      join3(step3, tail2._1, tail2._2);
                    }
                  };
                });
                if (tmp) {
                  tmp = false;
                  return;
                }
              }
              break;
          }
          if (tail2 === null) {
            head3 = null;
          } else {
            head3 = tail2._1;
            tail2 = tail2._2;
          }
        }
      }
      function resolve(fiber) {
        return function(result) {
          return function() {
            delete fibers[fiber._1];
            fiber._3 = result;
            join3(result, fiber._2._1, fiber._2._2);
          };
        };
      }
      function run3() {
        var status = CONTINUE;
        var step3 = par;
        var head3 = null;
        var tail2 = null;
        var tmp, fid;
        loop: while (true) {
          tmp = null;
          fid = null;
          switch (status) {
            case CONTINUE:
              switch (step3.tag) {
                case MAP:
                  if (head3) {
                    tail2 = new Aff2(CONS, head3, tail2);
                  }
                  head3 = new Aff2(MAP, step3._1, EMPTY, EMPTY);
                  step3 = step3._2;
                  break;
                case APPLY:
                  if (head3) {
                    tail2 = new Aff2(CONS, head3, tail2);
                  }
                  head3 = new Aff2(APPLY, EMPTY, step3._2, EMPTY);
                  step3 = step3._1;
                  break;
                case ALT:
                  if (head3) {
                    tail2 = new Aff2(CONS, head3, tail2);
                  }
                  head3 = new Aff2(ALT, EMPTY, step3._2, EMPTY);
                  step3 = step3._1;
                  break;
                default:
                  fid = fiberId++;
                  status = RETURN;
                  tmp = step3;
                  step3 = new Aff2(FORKED, fid, new Aff2(CONS, head3, tail2), EMPTY);
                  tmp = Fiber(util, supervisor, tmp);
                  tmp.onComplete({
                    rethrow: false,
                    handler: resolve(step3)
                  })();
                  fibers[fid] = tmp;
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
              }
              break;
            case RETURN:
              if (head3 === null) {
                break loop;
              }
              if (head3._1 === EMPTY) {
                head3._1 = step3;
                status = CONTINUE;
                step3 = head3._2;
                head3._2 = EMPTY;
              } else {
                head3._2 = step3;
                step3 = head3;
                if (tail2 === null) {
                  head3 = null;
                } else {
                  head3 = tail2._1;
                  tail2 = tail2._2;
                }
              }
          }
        }
        root = step3;
        for (fid = 0; fid < fiberId; fid++) {
          fibers[fid].run();
        }
      }
      function cancel(error4, cb2) {
        interrupt = util.left(error4);
        var innerKills;
        for (var kid in kills) {
          if (kills.hasOwnProperty(kid)) {
            innerKills = kills[kid];
            for (kid in innerKills) {
              if (innerKills.hasOwnProperty(kid)) {
                innerKills[kid]();
              }
            }
          }
        }
        kills = null;
        var newKills = kill2(error4, root, cb2);
        return function(killError) {
          return new Aff2(ASYNC, function(killCb) {
            return function() {
              for (var kid2 in newKills) {
                if (newKills.hasOwnProperty(kid2)) {
                  newKills[kid2]();
                }
              }
              return nonCanceler2;
            };
          });
        };
      }
      run3();
      return function(killError) {
        return new Aff2(ASYNC, function(killCb) {
          return function() {
            return cancel(killError, killCb);
          };
        });
      };
    }
    function sequential3(util, supervisor, par) {
      return new Aff2(ASYNC, function(cb) {
        return function() {
          return runPar(util, supervisor, par, cb);
        };
      });
    }
    Aff2.EMPTY = EMPTY;
    Aff2.Pure = AffCtr(PURE);
    Aff2.Throw = AffCtr(THROW);
    Aff2.Catch = AffCtr(CATCH);
    Aff2.Sync = AffCtr(SYNC);
    Aff2.Async = AffCtr(ASYNC);
    Aff2.Bind = AffCtr(BIND);
    Aff2.Bracket = AffCtr(BRACKET);
    Aff2.Fork = AffCtr(FORK);
    Aff2.Seq = AffCtr(SEQ);
    Aff2.ParMap = AffCtr(MAP);
    Aff2.ParApply = AffCtr(APPLY);
    Aff2.ParAlt = AffCtr(ALT);
    Aff2.Fiber = Fiber;
    Aff2.Supervisor = Supervisor;
    Aff2.Scheduler = Scheduler;
    Aff2.nonCanceler = nonCanceler2;
    return Aff2;
  }();
  var _pure = Aff.Pure;
  var _throwError = Aff.Throw;
  function _catchError(aff) {
    return function(k) {
      return Aff.Catch(aff, k);
    };
  }
  function _map(f) {
    return function(aff) {
      if (aff.tag === Aff.Pure.tag) {
        return Aff.Pure(f(aff._1));
      } else {
        return Aff.Bind(aff, function(value14) {
          return Aff.Pure(f(value14));
        });
      }
    };
  }
  function _bind(aff) {
    return function(k) {
      return Aff.Bind(aff, k);
    };
  }
  function _fork(immediate) {
    return function(aff) {
      return Aff.Fork(immediate, aff);
    };
  }
  var _liftEffect = Aff.Sync;
  function _parAffMap(f) {
    return function(aff) {
      return Aff.ParMap(f, aff);
    };
  }
  function _parAffApply(aff1) {
    return function(aff2) {
      return Aff.ParApply(aff1, aff2);
    };
  }
  var makeAff = Aff.Async;
  function generalBracket(acquire) {
    return function(options2) {
      return function(k) {
        return Aff.Bracket(acquire, options2, k);
      };
    };
  }
  function _makeFiber(util, aff) {
    return function() {
      return Aff.Fiber(util, null, aff);
    };
  }
  var _sequential = Aff.Seq;

  // output/Control.Monad/index.js
  var unlessM = function(dictMonad) {
    var bind16 = bind(dictMonad.Bind1());
    var unless2 = unless(dictMonad.Applicative0());
    return function(mb) {
      return function(m) {
        return bind16(mb)(function(b2) {
          return unless2(b2)(m);
        });
      };
    };
  };
  var ap = function(dictMonad) {
    var bind16 = bind(dictMonad.Bind1());
    var pure10 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a2) {
        return bind16(f)(function(f$prime) {
          return bind16(a2)(function(a$prime) {
            return pure10(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Data.Semigroup/foreign.js
  var concatArray = function(xs) {
    return function(ys) {
      if (xs.length === 0) return ys;
      if (ys.length === 0) return xs;
      return xs.concat(ys);
    };
  };

  // output/Data.Symbol/index.js
  var reflectSymbol = function(dict) {
    return dict.reflectSymbol;
  };

  // output/Record.Unsafe/foreign.js
  var unsafeGet = function(label5) {
    return function(rec) {
      return rec[label5];
    };
  };

  // output/Data.Semigroup/index.js
  var semigroupArray = {
    append: concatArray
  };
  var append = function(dict) {
    return dict.append;
  };

  // output/Control.Alt/index.js
  var alt = function(dict) {
    return dict.alt;
  };

  // output/Data.Bounded/foreign.js
  var topInt = 2147483647;
  var bottomInt = -2147483648;
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output/Data.Ord/foreign.js
  var unsafeCompareImpl = function(lt) {
    return function(eq3) {
      return function(gt) {
        return function(x) {
          return function(y) {
            return x < y ? lt : x === y ? eq3 : gt;
          };
        };
      };
    };
  };
  var ordIntImpl = unsafeCompareImpl;
  var ordNumberImpl = unsafeCompareImpl;
  var ordStringImpl = unsafeCompareImpl;
  var ordCharImpl = unsafeCompareImpl;

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqIntImpl = refEq;
  var eqNumberImpl = refEq;
  var eqCharImpl = refEq;
  var eqStringImpl = refEq;

  // output/Data.Eq/index.js
  var eqUnit = {
    eq: function(v) {
      return function(v1) {
        return true;
      };
    }
  };
  var eqString = {
    eq: eqStringImpl
  };
  var eqNumber = {
    eq: eqNumberImpl
  };
  var eqInt = {
    eq: eqIntImpl
  };
  var eqChar = {
    eq: eqCharImpl
  };
  var eq = function(dict) {
    return dict.eq;
  };

  // output/Data.Ordering/index.js
  var LT = /* @__PURE__ */ function() {
    function LT2() {
    }
    ;
    LT2.value = new LT2();
    return LT2;
  }();
  var GT = /* @__PURE__ */ function() {
    function GT2() {
    }
    ;
    GT2.value = new GT2();
    return GT2;
  }();
  var EQ = /* @__PURE__ */ function() {
    function EQ2() {
    }
    ;
    EQ2.value = new EQ2();
    return EQ2;
  }();

  // output/Data.Ord/index.js
  var ordUnit = {
    compare: function(v) {
      return function(v1) {
        return EQ.value;
      };
    },
    Eq0: function() {
      return eqUnit;
    }
  };
  var ordString = /* @__PURE__ */ function() {
    return {
      compare: ordStringImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqString;
      }
    };
  }();
  var ordNumber = /* @__PURE__ */ function() {
    return {
      compare: ordNumberImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqNumber;
      }
    };
  }();
  var ordInt = /* @__PURE__ */ function() {
    return {
      compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqInt;
      }
    };
  }();
  var ordChar = /* @__PURE__ */ function() {
    return {
      compare: ordCharImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqChar;
      }
    };
  }();
  var compare = function(dict) {
    return dict.compare;
  };
  var max = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(x) {
      return function(y) {
        var v = compare3(x)(y);
        if (v instanceof LT) {
          return y;
        }
        ;
        if (v instanceof EQ) {
          return x;
        }
        ;
        if (v instanceof GT) {
          return x;
        }
        ;
        throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): " + [v.constructor.name]);
      };
    };
  };
  var min = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(x) {
      return function(y) {
        var v = compare3(x)(y);
        if (v instanceof LT) {
          return x;
        }
        ;
        if (v instanceof EQ) {
          return x;
        }
        ;
        if (v instanceof GT) {
          return y;
        }
        ;
        throw new Error("Failed pattern match at Data.Ord (line 172, column 3 - line 175, column 12): " + [v.constructor.name]);
      };
    };
  };
  var clamp = function(dictOrd) {
    var min1 = min(dictOrd);
    var max1 = max(dictOrd);
    return function(low2) {
      return function(hi) {
        return function(x) {
          return min1(hi)(max1(low2)(x));
        };
      };
    };
  };

  // output/Data.Bounded/index.js
  var top = function(dict) {
    return dict.top;
  };
  var boundedInt = {
    top: topInt,
    bottom: bottomInt,
    Ord0: function() {
      return ordInt;
    }
  };
  var boundedChar = {
    top: topChar,
    bottom: bottomChar,
    Ord0: function() {
      return ordChar;
    }
  };
  var bottom = function(dict) {
    return dict.bottom;
  };

  // output/Data.Show/foreign.js
  var showNumberImpl = function(n) {
    var str = n.toString();
    return isNaN(str + ".0") ? str : str + ".0";
  };

  // output/Data.Show/index.js
  var showRecordFields = function(dict) {
    return dict.showRecordFields;
  };
  var showRecord = function() {
    return function() {
      return function(dictShowRecordFields) {
        var showRecordFields1 = showRecordFields(dictShowRecordFields);
        return {
          show: function(record) {
            return "{" + (showRecordFields1($$Proxy.value)(record) + "}");
          }
        };
      };
    };
  };
  var showNumber = {
    show: showNumberImpl
  };
  var show = function(dict) {
    return dict.show;
  };
  var showRecordFieldsCons = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function(dictShowRecordFields) {
      var showRecordFields1 = showRecordFields(dictShowRecordFields);
      return function(dictShow) {
        var show1 = show(dictShow);
        return {
          showRecordFields: function(v) {
            return function(record) {
              var tail2 = showRecordFields1($$Proxy.value)(record);
              var key5 = reflectSymbol2($$Proxy.value);
              var focus3 = unsafeGet(key5)(record);
              return " " + (key5 + (": " + (show1(focus3) + ("," + tail2))));
            };
          }
        };
      };
    };
  };
  var showRecordFieldsConsNil = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function(dictShow) {
      var show1 = show(dictShow);
      return {
        showRecordFields: function(v) {
          return function(record) {
            var key5 = reflectSymbol2($$Proxy.value);
            var focus3 = unsafeGet(key5)(record);
            return " " + (key5 + (": " + (show1(focus3) + " ")));
          };
        }
      };
    };
  };

  // output/Data.Maybe/index.js
  var identity3 = /* @__PURE__ */ identity(categoryFn);
  var Nothing = /* @__PURE__ */ function() {
    function Nothing2() {
    }
    ;
    Nothing2.value = new Nothing2();
    return Nothing2;
  }();
  var Just = /* @__PURE__ */ function() {
    function Just2(value0) {
      this.value0 = value0;
    }
    ;
    Just2.create = function(value0) {
      return new Just2(value0);
    };
    return Just2;
  }();
  var maybe = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v;
        }
        ;
        if (v2 instanceof Just) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var isNothing = /* @__PURE__ */ maybe(true)(/* @__PURE__ */ $$const(false));
  var isJust = /* @__PURE__ */ maybe(false)(/* @__PURE__ */ $$const(true));
  var functorMaybe = {
    map: function(v) {
      return function(v1) {
        if (v1 instanceof Just) {
          return new Just(v(v1.value0));
        }
        ;
        return Nothing.value;
      };
    }
  };
  var map2 = /* @__PURE__ */ map(functorMaybe);
  var fromMaybe = function(a2) {
    return maybe(a2)(identity3);
  };
  var fromJust = function() {
    return function(v) {
      if (v instanceof Just) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
    };
  };
  var eqMaybe = function(dictEq) {
    var eq3 = eq(dictEq);
    return {
      eq: function(x) {
        return function(y) {
          if (x instanceof Nothing && y instanceof Nothing) {
            return true;
          }
          ;
          if (x instanceof Just && y instanceof Just) {
            return eq3(x.value0)(y.value0);
          }
          ;
          return false;
        };
      }
    };
  };
  var applyMaybe = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return map2(v.value0)(v1);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorMaybe;
    }
  };
  var bindMaybe = {
    bind: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return v1(v.value0);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Apply0: function() {
      return applyMaybe;
    }
  };

  // output/Data.Either/index.js
  var Left = /* @__PURE__ */ function() {
    function Left2(value0) {
      this.value0 = value0;
    }
    ;
    Left2.create = function(value0) {
      return new Left2(value0);
    };
    return Left2;
  }();
  var Right = /* @__PURE__ */ function() {
    function Right2(value0) {
      this.value0 = value0;
    }
    ;
    Right2.create = function(value0) {
      return new Right2(value0);
    };
    return Right2;
  }();
  var functorEither = {
    map: function(f) {
      return function(m) {
        if (m instanceof Left) {
          return new Left(m.value0);
        }
        ;
        if (m instanceof Right) {
          return new Right(f(m.value0));
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
      };
    }
  };
  var either = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Left) {
          return v(v2.value0);
        }
        ;
        if (v2 instanceof Right) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };

  // output/Effect/foreign.js
  var pureE = function(a2) {
    return function() {
      return a2;
    };
  };
  var bindE = function(a2) {
    return function(f) {
      return function() {
        return f(a2())();
      };
    };
  };

  // output/Data.Monoid/index.js
  var monoidArray = {
    mempty: [],
    Semigroup0: function() {
      return semigroupArray;
    }
  };
  var mempty = function(dict) {
    return dict.mempty;
  };

  // output/Effect/index.js
  var $runtime_lazy = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var monadEffect = {
    Applicative0: function() {
      return applicativeEffect;
    },
    Bind1: function() {
      return bindEffect;
    }
  };
  var bindEffect = {
    bind: bindE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var applicativeEffect = {
    pure: pureE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy("functorEffect", "Effect", function() {
    return {
      map: liftA1(applicativeEffect)
    };
  });
  var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy("applyEffect", "Effect", function() {
    return {
      apply: ap(monadEffect),
      Functor0: function() {
        return $lazy_functorEffect(0);
      }
    };
  });
  var functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);
  var applyEffect = /* @__PURE__ */ $lazy_applyEffect(23);

  // output/Effect.Exception/foreign.js
  function error(msg) {
    return new Error(msg);
  }
  function throwException(e) {
    return function() {
      throw e;
    };
  }

  // output/Effect.Exception/index.js
  var $$throw = function($4) {
    return throwException(error($4));
  };

  // output/Control.Monad.Error.Class/index.js
  var throwError = function(dict) {
    return dict.throwError;
  };
  var catchError = function(dict) {
    return dict.catchError;
  };
  var $$try = function(dictMonadError) {
    var catchError1 = catchError(dictMonadError);
    var Monad0 = dictMonadError.MonadThrow0().Monad0();
    var map28 = map(Monad0.Bind1().Apply0().Functor0());
    var pure10 = pure(Monad0.Applicative0());
    return function(a2) {
      return catchError1(map28(Right.create)(a2))(function($52) {
        return pure10(Left.create($52));
      });
    };
  };

  // output/Data.Identity/index.js
  var Identity = function(x) {
    return x;
  };
  var functorIdentity = {
    map: function(f) {
      return function(m) {
        return f(m);
      };
    }
  };
  var applyIdentity = {
    apply: function(v) {
      return function(v1) {
        return v(v1);
      };
    },
    Functor0: function() {
      return functorIdentity;
    }
  };
  var bindIdentity = {
    bind: function(v) {
      return function(f) {
        return f(v);
      };
    },
    Apply0: function() {
      return applyIdentity;
    }
  };
  var applicativeIdentity = {
    pure: Identity,
    Apply0: function() {
      return applyIdentity;
    }
  };
  var monadIdentity = {
    Applicative0: function() {
      return applicativeIdentity;
    },
    Bind1: function() {
      return bindIdentity;
    }
  };

  // output/Effect.Ref/foreign.js
  var _new = function(val) {
    return function() {
      return { value: val };
    };
  };
  var read = function(ref3) {
    return function() {
      return ref3.value;
    };
  };
  var modifyImpl = function(f) {
    return function(ref3) {
      return function() {
        var t = f(ref3.value);
        ref3.value = t.state;
        return t.value;
      };
    };
  };
  var write = function(val) {
    return function(ref3) {
      return function() {
        ref3.value = val;
      };
    };
  };

  // output/Effect.Ref/index.js
  var $$void2 = /* @__PURE__ */ $$void(functorEffect);
  var $$new = _new;
  var modify$prime = modifyImpl;
  var modify = function(f) {
    return modify$prime(function(s) {
      var s$prime = f(s);
      return {
        state: s$prime,
        value: s$prime
      };
    });
  };
  var modify_ = function(f) {
    return function(s) {
      return $$void2(modify(f)(s));
    };
  };

  // output/Control.Monad.Rec.Class/index.js
  var bindFlipped2 = /* @__PURE__ */ bindFlipped(bindEffect);
  var map3 = /* @__PURE__ */ map(functorEffect);
  var Loop = /* @__PURE__ */ function() {
    function Loop2(value0) {
      this.value0 = value0;
    }
    ;
    Loop2.create = function(value0) {
      return new Loop2(value0);
    };
    return Loop2;
  }();
  var Done = /* @__PURE__ */ function() {
    function Done2(value0) {
      this.value0 = value0;
    }
    ;
    Done2.create = function(value0) {
      return new Done2(value0);
    };
    return Done2;
  }();
  var tailRecM = function(dict) {
    return dict.tailRecM;
  };
  var monadRecEffect = {
    tailRecM: function(f) {
      return function(a2) {
        var fromDone = function(v) {
          if (v instanceof Done) {
            return v.value0;
          }
          ;
          throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 137, column 30 - line 137, column 44): " + [v.constructor.name]);
        };
        return function __do2() {
          var r = bindFlipped2($$new)(f(a2))();
          (function() {
            while (!function __do3() {
              var v = read(r)();
              if (v instanceof Loop) {
                var e = f(v.value0)();
                write(e)(r)();
                return false;
              }
              ;
              if (v instanceof Done) {
                return true;
              }
              ;
              throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 128, column 22 - line 133, column 28): " + [v.constructor.name]);
            }()) {
            }
            ;
            return {};
          })();
          return map3(fromDone)(read(r))();
        };
      };
    },
    Monad0: function() {
      return monadEffect;
    }
  };

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x) {
    return x;
  };

  // output/Control.Monad.ST.Internal/foreign.js
  var map_ = function(f) {
    return function(a2) {
      return function() {
        return f(a2());
      };
    };
  };
  var foreach = function(as) {
    return function(f) {
      return function() {
        for (var i2 = 0, l = as.length; i2 < l; i2++) {
          f(as[i2])();
        }
      };
    };
  };

  // output/Control.Monad.ST.Internal/index.js
  var functorST = {
    map: map_
  };

  // output/Control.Monad.Reader.Class/index.js
  var ask = function(dict) {
    return dict.ask;
  };

  // output/Data.HeytingAlgebra/foreign.js
  var boolConj = function(b1) {
    return function(b2) {
      return b1 && b2;
    };
  };
  var boolDisj = function(b1) {
    return function(b2) {
      return b1 || b2;
    };
  };
  var boolNot = function(b2) {
    return !b2;
  };

  // output/Data.HeytingAlgebra/index.js
  var tt = function(dict) {
    return dict.tt;
  };
  var not = function(dict) {
    return dict.not;
  };
  var implies = function(dict) {
    return dict.implies;
  };
  var ff = function(dict) {
    return dict.ff;
  };
  var disj = function(dict) {
    return dict.disj;
  };
  var heytingAlgebraBoolean = {
    ff: false,
    tt: true,
    implies: function(a2) {
      return function(b2) {
        return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a2))(b2);
      };
    },
    conj: boolConj,
    disj: boolDisj,
    not: boolNot
  };
  var conj = function(dict) {
    return dict.conj;
  };
  var heytingAlgebraFunction = function(dictHeytingAlgebra) {
    var ff1 = ff(dictHeytingAlgebra);
    var tt1 = tt(dictHeytingAlgebra);
    var implies1 = implies(dictHeytingAlgebra);
    var conj1 = conj(dictHeytingAlgebra);
    var disj1 = disj(dictHeytingAlgebra);
    var not1 = not(dictHeytingAlgebra);
    return {
      ff: function(v) {
        return ff1;
      },
      tt: function(v) {
        return tt1;
      },
      implies: function(f) {
        return function(g) {
          return function(a2) {
            return implies1(f(a2))(g(a2));
          };
        };
      },
      conj: function(f) {
        return function(g) {
          return function(a2) {
            return conj1(f(a2))(g(a2));
          };
        };
      },
      disj: function(f) {
        return function(g) {
          return function(a2) {
            return disj1(f(a2))(g(a2));
          };
        };
      },
      not: function(f) {
        return function(a2) {
          return not1(f(a2));
        };
      }
    };
  };

  // output/Data.Tuple/index.js
  var Tuple = /* @__PURE__ */ function() {
    function Tuple2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Tuple2.create = function(value0) {
      return function(value1) {
        return new Tuple2(value0, value1);
      };
    };
    return Tuple2;
  }();
  var snd = function(v) {
    return v.value1;
  };
  var functorTuple = {
    map: function(f) {
      return function(m) {
        return new Tuple(m.value0, f(m.value1));
      };
    }
  };
  var fst = function(v) {
    return v.value0;
  };
  var eqTuple = function(dictEq) {
    var eq3 = eq(dictEq);
    return function(dictEq1) {
      var eq12 = eq(dictEq1);
      return {
        eq: function(x) {
          return function(y) {
            return eq3(x.value0)(y.value0) && eq12(x.value1)(y.value1);
          };
        }
      };
    };
  };
  var ordTuple = function(dictOrd) {
    var compare2 = compare(dictOrd);
    var eqTuple1 = eqTuple(dictOrd.Eq0());
    return function(dictOrd1) {
      var compare12 = compare(dictOrd1);
      var eqTuple2 = eqTuple1(dictOrd1.Eq0());
      return {
        compare: function(x) {
          return function(y) {
            var v = compare2(x.value0)(y.value0);
            if (v instanceof LT) {
              return LT.value;
            }
            ;
            if (v instanceof GT) {
              return GT.value;
            }
            ;
            return compare12(x.value1)(y.value1);
          };
        },
        Eq0: function() {
          return eqTuple2;
        }
      };
    };
  };

  // output/Control.Monad.State.Class/index.js
  var state = function(dict) {
    return dict.state;
  };
  var modify_2 = function(dictMonadState) {
    var state1 = state(dictMonadState);
    return function(f) {
      return state1(function(s) {
        return new Tuple(unit, f(s));
      });
    };
  };
  var modify2 = function(dictMonadState) {
    var state1 = state(dictMonadState);
    return function(f) {
      return state1(function(s) {
        var s$prime = f(s);
        return new Tuple(s$prime, s$prime);
      });
    };
  };
  var get = function(dictMonadState) {
    return state(dictMonadState)(function(s) {
      return new Tuple(s, s);
    });
  };

  // output/Control.Monad.Trans.Class/index.js
  var lift = function(dict) {
    return dict.lift;
  };

  // output/Effect.Class/index.js
  var monadEffectEffect = {
    liftEffect: /* @__PURE__ */ identity(categoryFn),
    Monad0: function() {
      return monadEffect;
    }
  };
  var liftEffect = function(dict) {
    return dict.liftEffect;
  };

  // output/Control.Monad.Writer.Class/index.js
  var tell = function(dict) {
    return dict.tell;
  };

  // output/Control.Monad.Except.Trans/index.js
  var map4 = /* @__PURE__ */ map(functorEither);
  var ExceptT = function(x) {
    return x;
  };
  var runExceptT = function(v) {
    return v;
  };
  var mapExceptT = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var functorExceptT = function(dictFunctor) {
    var map112 = map(dictFunctor);
    return {
      map: function(f) {
        return mapExceptT(map112(map4(f)));
      }
    };
  };
  var monadExceptT = function(dictMonad) {
    return {
      Applicative0: function() {
        return applicativeExceptT(dictMonad);
      },
      Bind1: function() {
        return bindExceptT(dictMonad);
      }
    };
  };
  var bindExceptT = function(dictMonad) {
    var bind16 = bind(dictMonad.Bind1());
    var pure10 = pure(dictMonad.Applicative0());
    return {
      bind: function(v) {
        return function(k) {
          return bind16(v)(either(function($193) {
            return pure10(Left.create($193));
          })(function(a2) {
            var v1 = k(a2);
            return v1;
          }));
        };
      },
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var applyExceptT = function(dictMonad) {
    var functorExceptT1 = functorExceptT(dictMonad.Bind1().Apply0().Functor0());
    return {
      apply: ap(monadExceptT(dictMonad)),
      Functor0: function() {
        return functorExceptT1;
      }
    };
  };
  var applicativeExceptT = function(dictMonad) {
    return {
      pure: function() {
        var $194 = pure(dictMonad.Applicative0());
        return function($195) {
          return ExceptT($194(Right.create($195)));
        };
      }(),
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var monadThrowExceptT = function(dictMonad) {
    var monadExceptT1 = monadExceptT(dictMonad);
    return {
      throwError: function() {
        var $204 = pure(dictMonad.Applicative0());
        return function($205) {
          return ExceptT($204(Left.create($205)));
        };
      }(),
      Monad0: function() {
        return monadExceptT1;
      }
    };
  };
  var altExceptT = function(dictSemigroup) {
    var append8 = append(dictSemigroup);
    return function(dictMonad) {
      var Bind1 = dictMonad.Bind1();
      var bind16 = bind(Bind1);
      var pure10 = pure(dictMonad.Applicative0());
      var functorExceptT1 = functorExceptT(Bind1.Apply0().Functor0());
      return {
        alt: function(v) {
          return function(v1) {
            return bind16(v)(function(rm) {
              if (rm instanceof Right) {
                return pure10(new Right(rm.value0));
              }
              ;
              if (rm instanceof Left) {
                return bind16(v1)(function(rn) {
                  if (rn instanceof Right) {
                    return pure10(new Right(rn.value0));
                  }
                  ;
                  if (rn instanceof Left) {
                    return pure10(new Left(append8(rm.value0)(rn.value0)));
                  }
                  ;
                  throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 87, column 9 - line 89, column 49): " + [rn.constructor.name]);
                });
              }
              ;
              throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 83, column 5 - line 89, column 49): " + [rm.constructor.name]);
            });
          };
        },
        Functor0: function() {
          return functorExceptT1;
        }
      };
    };
  };

  // output/Control.Plus/index.js
  var empty = function(dict) {
    return dict.empty;
  };

  // output/Safe.Coerce/index.js
  var coerce = function() {
    return unsafeCoerce2;
  };

  // output/Data.Newtype/index.js
  var coerce2 = /* @__PURE__ */ coerce();
  var unwrap = function() {
    return coerce2;
  };

  // output/Control.Monad.Reader.Trans/index.js
  var ReaderT = function(x) {
    return x;
  };
  var runReaderT = function(v) {
    return v;
  };
  var monadTransReaderT = {
    lift: function(dictMonad) {
      return function($153) {
        return ReaderT($$const($153));
      };
    }
  };
  var lift3 = /* @__PURE__ */ lift(monadTransReaderT);
  var mapReaderT = function(f) {
    return function(v) {
      return function($154) {
        return f(v($154));
      };
    };
  };
  var functorReaderT = function(dictFunctor) {
    return {
      map: function() {
        var $155 = map(dictFunctor);
        return function($156) {
          return mapReaderT($155($156));
        };
      }()
    };
  };
  var applyReaderT = function(dictApply) {
    var apply3 = apply(dictApply);
    var functorReaderT1 = functorReaderT(dictApply.Functor0());
    return {
      apply: function(v) {
        return function(v1) {
          return function(r) {
            return apply3(v(r))(v1(r));
          };
        };
      },
      Functor0: function() {
        return functorReaderT1;
      }
    };
  };
  var bindReaderT = function(dictBind) {
    var bind16 = bind(dictBind);
    var applyReaderT1 = applyReaderT(dictBind.Apply0());
    return {
      bind: function(v) {
        return function(k) {
          return function(r) {
            return bind16(v(r))(function(a2) {
              var v1 = k(a2);
              return v1(r);
            });
          };
        };
      },
      Apply0: function() {
        return applyReaderT1;
      }
    };
  };
  var applicativeReaderT = function(dictApplicative) {
    var applyReaderT1 = applyReaderT(dictApplicative.Apply0());
    return {
      pure: function() {
        var $160 = pure(dictApplicative);
        return function($161) {
          return ReaderT($$const($160($161)));
        };
      }(),
      Apply0: function() {
        return applyReaderT1;
      }
    };
  };
  var monadReaderT = function(dictMonad) {
    var applicativeReaderT1 = applicativeReaderT(dictMonad.Applicative0());
    var bindReaderT1 = bindReaderT(dictMonad.Bind1());
    return {
      Applicative0: function() {
        return applicativeReaderT1;
      },
      Bind1: function() {
        return bindReaderT1;
      }
    };
  };
  var monadAskReaderT = function(dictMonad) {
    var monadReaderT1 = monadReaderT(dictMonad);
    return {
      ask: pure(dictMonad.Applicative0()),
      Monad0: function() {
        return monadReaderT1;
      }
    };
  };
  var monadEffectReader = function(dictMonadEffect) {
    var Monad0 = dictMonadEffect.Monad0();
    var monadReaderT1 = monadReaderT(Monad0);
    return {
      liftEffect: function() {
        var $163 = lift3(Monad0);
        var $164 = liftEffect(dictMonadEffect);
        return function($165) {
          return $163($164($165));
        };
      }(),
      Monad0: function() {
        return monadReaderT1;
      }
    };
  };
  var monadRecReaderT = function(dictMonadRec) {
    var Monad0 = dictMonadRec.Monad0();
    var bindFlipped8 = bindFlipped(Monad0.Bind1());
    var pure10 = pure(Monad0.Applicative0());
    var tailRecM4 = tailRecM(dictMonadRec);
    var monadReaderT1 = monadReaderT(Monad0);
    return {
      tailRecM: function(k) {
        return function(a2) {
          var k$prime = function(r) {
            return function(a$prime) {
              var v = k(a$prime);
              return bindFlipped8(pure10)(v(r));
            };
          };
          return function(r) {
            return tailRecM4(k$prime(r))(a2);
          };
        };
      },
      Monad0: function() {
        return monadReaderT1;
      }
    };
  };

  // output/Control.Monad.Writer.Trans/index.js
  var WriterT = function(x) {
    return x;
  };
  var runWriterT = function(v) {
    return v;
  };
  var mapWriterT = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var functorWriterT = function(dictFunctor) {
    var map28 = map(dictFunctor);
    return {
      map: function(f) {
        return mapWriterT(map28(function(v) {
          return new Tuple(f(v.value0), v.value1);
        }));
      }
    };
  };
  var applyWriterT = function(dictSemigroup) {
    var append8 = append(dictSemigroup);
    return function(dictApply) {
      var apply3 = apply(dictApply);
      var Functor0 = dictApply.Functor0();
      var map28 = map(Functor0);
      var functorWriterT1 = functorWriterT(Functor0);
      return {
        apply: function(v) {
          return function(v1) {
            var k = function(v3) {
              return function(v4) {
                return new Tuple(v3.value0(v4.value0), append8(v3.value1)(v4.value1));
              };
            };
            return apply3(map28(k)(v))(v1);
          };
        },
        Functor0: function() {
          return functorWriterT1;
        }
      };
    };
  };
  var bindWriterT = function(dictSemigroup) {
    var append8 = append(dictSemigroup);
    var applyWriterT1 = applyWriterT(dictSemigroup);
    return function(dictBind) {
      var bind16 = bind(dictBind);
      var Apply0 = dictBind.Apply0();
      var map28 = map(Apply0.Functor0());
      var applyWriterT2 = applyWriterT1(Apply0);
      return {
        bind: function(v) {
          return function(k) {
            return bind16(v)(function(v1) {
              var v2 = k(v1.value0);
              return map28(function(v3) {
                return new Tuple(v3.value0, append8(v1.value1)(v3.value1));
              })(v2);
            });
          };
        },
        Apply0: function() {
          return applyWriterT2;
        }
      };
    };
  };
  var applicativeWriterT = function(dictMonoid) {
    var mempty3 = mempty(dictMonoid);
    var applyWriterT1 = applyWriterT(dictMonoid.Semigroup0());
    return function(dictApplicative) {
      var pure10 = pure(dictApplicative);
      var applyWriterT2 = applyWriterT1(dictApplicative.Apply0());
      return {
        pure: function(a2) {
          return pure10(new Tuple(a2, mempty3));
        },
        Apply0: function() {
          return applyWriterT2;
        }
      };
    };
  };
  var monadWriterT = function(dictMonoid) {
    var applicativeWriterT1 = applicativeWriterT(dictMonoid);
    var bindWriterT1 = bindWriterT(dictMonoid.Semigroup0());
    return function(dictMonad) {
      var applicativeWriterT2 = applicativeWriterT1(dictMonad.Applicative0());
      var bindWriterT2 = bindWriterT1(dictMonad.Bind1());
      return {
        Applicative0: function() {
          return applicativeWriterT2;
        },
        Bind1: function() {
          return bindWriterT2;
        }
      };
    };
  };
  var monadTellWriterT = function(dictMonoid) {
    var Semigroup0 = dictMonoid.Semigroup0();
    var monadWriterT1 = monadWriterT(dictMonoid);
    return function(dictMonad) {
      var monadWriterT2 = monadWriterT1(dictMonad);
      return {
        tell: function() {
          var $262 = pure(dictMonad.Applicative0());
          var $263 = Tuple.create(unit);
          return function($264) {
            return WriterT($262($263($264)));
          };
        }(),
        Semigroup0: function() {
          return Semigroup0;
        },
        Monad1: function() {
          return monadWriterT2;
        }
      };
    };
  };

  // output/Data.Profunctor/index.js
  var profunctorFn = {
    dimap: function(a2b) {
      return function(c2d) {
        return function(b2c) {
          return function($18) {
            return c2d(b2c(a2b($18)));
          };
        };
      };
    }
  };

  // output/Control.Parallel.Class/index.js
  var sequential = function(dict) {
    return dict.sequential;
  };
  var parallel = function(dict) {
    return dict.parallel;
  };

  // output/Data.Foldable/foreign.js
  var foldrArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i2 = len - 1; i2 >= 0; i2--) {
          acc = f(xs[i2])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i2 = 0; i2 < len; i2++) {
          acc = f(acc)(xs[i2]);
        }
        return acc;
      };
    };
  };

  // output/Data.Bifunctor/index.js
  var bimap = function(dict) {
    return dict.bimap;
  };

  // output/Data.Maybe.First/index.js
  var semigroupFirst = {
    append: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return v;
        }
        ;
        return v1;
      };
    }
  };
  var monoidFirst = /* @__PURE__ */ function() {
    return {
      mempty: Nothing.value,
      Semigroup0: function() {
        return semigroupFirst;
      }
    };
  }();

  // output/Data.Foldable/index.js
  var unwrap2 = /* @__PURE__ */ unwrap();
  var foldr = function(dict) {
    return dict.foldr;
  };
  var traverse_ = function(dictApplicative) {
    var applySecond3 = applySecond(dictApplicative.Apply0());
    var pure10 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr22 = foldr(dictFoldable);
      return function(f) {
        return foldr22(function($454) {
          return applySecond3(f($454));
        })(pure10(unit));
      };
    };
  };
  var for_ = function(dictApplicative) {
    var traverse_14 = traverse_(dictApplicative);
    return function(dictFoldable) {
      return flip(traverse_14(dictFoldable));
    };
  };
  var foldl = function(dict) {
    return dict.foldl;
  };
  var foldableMaybe = {
    foldr: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return v1;
          }
          ;
          if (v2 instanceof Just) {
            return v(v2.value0)(v1);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldl: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return v1;
          }
          ;
          if (v2 instanceof Just) {
            return v(v1)(v2.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty3 = mempty(dictMonoid);
      return function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return mempty3;
          }
          ;
          if (v1 instanceof Just) {
            return v(v1.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name]);
        };
      };
    }
  };
  var foldableEither = {
    foldr: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Left) {
            return v1;
          }
          ;
          if (v2 instanceof Right) {
            return v(v2.value0)(v1);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 181, column 1 - line 187, column 28): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldl: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Left) {
            return v1;
          }
          ;
          if (v2 instanceof Right) {
            return v(v1)(v2.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 181, column 1 - line 187, column 28): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty3 = mempty(dictMonoid);
      return function(v) {
        return function(v1) {
          if (v1 instanceof Left) {
            return mempty3;
          }
          ;
          if (v1 instanceof Right) {
            return v(v1.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 181, column 1 - line 187, column 28): " + [v.constructor.name, v1.constructor.name]);
        };
      };
    }
  };
  var foldMapDefaultR = function(dictFoldable) {
    var foldr22 = foldr(dictFoldable);
    return function(dictMonoid) {
      var append8 = append(dictMonoid.Semigroup0());
      var mempty3 = mempty(dictMonoid);
      return function(f) {
        return foldr22(function(x) {
          return function(acc) {
            return append8(f(x))(acc);
          };
        })(mempty3);
      };
    };
  };
  var foldableArray = {
    foldr: foldrArray,
    foldl: foldlArray,
    foldMap: function(dictMonoid) {
      return foldMapDefaultR(foldableArray)(dictMonoid);
    }
  };
  var foldMap = function(dict) {
    return dict.foldMap;
  };
  var lookup = function(dictFoldable) {
    var foldMap22 = foldMap(dictFoldable)(monoidFirst);
    return function(dictEq) {
      var eq22 = eq(dictEq);
      return function(a2) {
        var $460 = foldMap22(function(v) {
          var $444 = eq22(a2)(v.value0);
          if ($444) {
            return new Just(v.value1);
          }
          ;
          return Nothing.value;
        });
        return function($461) {
          return unwrap2($460($461));
        };
      };
    };
  };

  // output/Data.Traversable/foreign.js
  var traverseArrayImpl = /* @__PURE__ */ function() {
    function array1(a2) {
      return [a2];
    }
    function array2(a2) {
      return function(b2) {
        return [a2, b2];
      };
    }
    function array3(a2) {
      return function(b2) {
        return function(c) {
          return [a2, b2, c];
        };
      };
    }
    function concat2(xs) {
      return function(ys) {
        return xs.concat(ys);
      };
    }
    return function(apply3) {
      return function(map28) {
        return function(pure10) {
          return function(f) {
            return function(array) {
              function go2(bot, top3) {
                switch (top3 - bot) {
                  case 0:
                    return pure10([]);
                  case 1:
                    return map28(array1)(f(array[bot]));
                  case 2:
                    return apply3(map28(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply3(apply3(map28(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top3 - bot) / 4) * 2;
                    return apply3(map28(concat2)(go2(bot, pivot)))(go2(pivot, top3));
                }
              }
              return go2(0, array.length);
            };
          };
        };
      };
    };
  }();

  // output/Data.Traversable/index.js
  var identity4 = /* @__PURE__ */ identity(categoryFn);
  var traverse = function(dict) {
    return dict.traverse;
  };
  var traversableMaybe = {
    traverse: function(dictApplicative) {
      var pure10 = pure(dictApplicative);
      var map28 = map(dictApplicative.Apply0().Functor0());
      return function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return pure10(Nothing.value);
          }
          ;
          if (v1 instanceof Just) {
            return map28(Just.create)(v(v1.value0));
          }
          ;
          throw new Error("Failed pattern match at Data.Traversable (line 115, column 1 - line 119, column 33): " + [v.constructor.name, v1.constructor.name]);
        };
      };
    },
    sequence: function(dictApplicative) {
      var pure10 = pure(dictApplicative);
      var map28 = map(dictApplicative.Apply0().Functor0());
      return function(v) {
        if (v instanceof Nothing) {
          return pure10(Nothing.value);
        }
        ;
        if (v instanceof Just) {
          return map28(Just.create)(v.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Traversable (line 115, column 1 - line 119, column 33): " + [v.constructor.name]);
      };
    },
    Functor0: function() {
      return functorMaybe;
    },
    Foldable1: function() {
      return foldableMaybe;
    }
  };
  var sequenceDefault = function(dictTraversable) {
    var traverse22 = traverse(dictTraversable);
    return function(dictApplicative) {
      return traverse22(dictApplicative)(identity4);
    };
  };
  var traversableArray = {
    traverse: function(dictApplicative) {
      var Apply0 = dictApplicative.Apply0();
      return traverseArrayImpl(apply(Apply0))(map(Apply0.Functor0()))(pure(dictApplicative));
    },
    sequence: function(dictApplicative) {
      return sequenceDefault(traversableArray)(dictApplicative);
    },
    Functor0: function() {
      return functorArray;
    },
    Foldable1: function() {
      return foldableArray;
    }
  };
  var sequence = function(dict) {
    return dict.sequence;
  };

  // output/Control.Parallel/index.js
  var identity5 = /* @__PURE__ */ identity(categoryFn);
  var parTraverse_ = function(dictParallel) {
    var sequential3 = sequential(dictParallel);
    var parallel4 = parallel(dictParallel);
    return function(dictApplicative) {
      var traverse_9 = traverse_(dictApplicative);
      return function(dictFoldable) {
        var traverse_14 = traverse_9(dictFoldable);
        return function(f) {
          var $51 = traverse_14(function($53) {
            return parallel4(f($53));
          });
          return function($52) {
            return sequential3($51($52));
          };
        };
      };
    };
  };
  var parSequence_ = function(dictParallel) {
    var parTraverse_1 = parTraverse_(dictParallel);
    return function(dictApplicative) {
      var parTraverse_2 = parTraverse_1(dictApplicative);
      return function(dictFoldable) {
        return parTraverse_2(dictFoldable)(identity5);
      };
    };
  };

  // output/Effect.Unsafe/foreign.js
  var unsafePerformEffect = function(f) {
    return f();
  };

  // output/Partial.Unsafe/foreign.js
  var _unsafePartial = function(f) {
    return f();
  };

  // output/Partial/foreign.js
  var _crashWith = function(msg) {
    throw new Error(msg);
  };

  // output/Partial/index.js
  var crashWith = function() {
    return _crashWith;
  };

  // output/Partial.Unsafe/index.js
  var crashWith2 = /* @__PURE__ */ crashWith();
  var unsafePartial = _unsafePartial;
  var unsafeCrashWith = function(msg) {
    return unsafePartial(function() {
      return crashWith2(msg);
    });
  };

  // output/Effect.Aff/index.js
  var $runtime_lazy2 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var pure2 = /* @__PURE__ */ pure(applicativeEffect);
  var $$void3 = /* @__PURE__ */ $$void(functorEffect);
  var map5 = /* @__PURE__ */ map(functorEffect);
  var Canceler = function(x) {
    return x;
  };
  var suspendAff = /* @__PURE__ */ _fork(false);
  var functorParAff = {
    map: _parAffMap
  };
  var functorAff = {
    map: _map
  };
  var map1 = /* @__PURE__ */ map(functorAff);
  var forkAff = /* @__PURE__ */ _fork(true);
  var ffiUtil = /* @__PURE__ */ function() {
    var unsafeFromRight = function(v) {
      if (v instanceof Right) {
        return v.value0;
      }
      ;
      if (v instanceof Left) {
        return unsafeCrashWith("unsafeFromRight: Left");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 412, column 21 - line 414, column 54): " + [v.constructor.name]);
    };
    var unsafeFromLeft = function(v) {
      if (v instanceof Left) {
        return v.value0;
      }
      ;
      if (v instanceof Right) {
        return unsafeCrashWith("unsafeFromLeft: Right");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 407, column 20 - line 409, column 55): " + [v.constructor.name]);
    };
    var isLeft = function(v) {
      if (v instanceof Left) {
        return true;
      }
      ;
      if (v instanceof Right) {
        return false;
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 402, column 12 - line 404, column 21): " + [v.constructor.name]);
    };
    return {
      isLeft,
      fromLeft: unsafeFromLeft,
      fromRight: unsafeFromRight,
      left: Left.create,
      right: Right.create
    };
  }();
  var makeFiber = function(aff) {
    return _makeFiber(ffiUtil, aff);
  };
  var launchAff = function(aff) {
    return function __do2() {
      var fiber = makeFiber(aff)();
      fiber.run();
      return fiber;
    };
  };
  var bracket = function(acquire) {
    return function(completed) {
      return generalBracket(acquire)({
        killed: $$const(completed),
        failed: $$const(completed),
        completed: $$const(completed)
      });
    };
  };
  var applyParAff = {
    apply: _parAffApply,
    Functor0: function() {
      return functorParAff;
    }
  };
  var monadAff = {
    Applicative0: function() {
      return applicativeAff;
    },
    Bind1: function() {
      return bindAff;
    }
  };
  var bindAff = {
    bind: _bind,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var applicativeAff = {
    pure: _pure,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var $lazy_applyAff = /* @__PURE__ */ $runtime_lazy2("applyAff", "Effect.Aff", function() {
    return {
      apply: ap(monadAff),
      Functor0: function() {
        return functorAff;
      }
    };
  });
  var applyAff = /* @__PURE__ */ $lazy_applyAff(73);
  var pure22 = /* @__PURE__ */ pure(applicativeAff);
  var bind1 = /* @__PURE__ */ bind(bindAff);
  var bindFlipped3 = /* @__PURE__ */ bindFlipped(bindAff);
  var $$finally = function(fin) {
    return function(a2) {
      return bracket(pure22(unit))($$const(fin))($$const(a2));
    };
  };
  var parallelAff = {
    parallel: unsafeCoerce2,
    sequential: _sequential,
    Apply0: function() {
      return applyAff;
    },
    Apply1: function() {
      return applyParAff;
    }
  };
  var parallel2 = /* @__PURE__ */ parallel(parallelAff);
  var applicativeParAff = {
    pure: function($76) {
      return parallel2(pure22($76));
    },
    Apply0: function() {
      return applyParAff;
    }
  };
  var parSequence_2 = /* @__PURE__ */ parSequence_(parallelAff)(applicativeParAff)(foldableArray);
  var semigroupCanceler = {
    append: function(v) {
      return function(v1) {
        return function(err) {
          return parSequence_2([v(err), v1(err)]);
        };
      };
    }
  };
  var monadEffectAff = {
    liftEffect: _liftEffect,
    Monad0: function() {
      return monadAff;
    }
  };
  var liftEffect2 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var effectCanceler = function($77) {
    return Canceler($$const(liftEffect2($77)));
  };
  var joinFiber = function(v) {
    return makeAff(function(k) {
      return map5(effectCanceler)(v.join(k));
    });
  };
  var functorFiber = {
    map: function(f) {
      return function(t) {
        return unsafePerformEffect(makeFiber(map1(f)(joinFiber(t))));
      };
    }
  };
  var killFiber = function(e) {
    return function(v) {
      return bind1(liftEffect2(v.isSuspended))(function(suspended) {
        if (suspended) {
          return liftEffect2($$void3(v.kill(e, $$const(pure2(unit)))));
        }
        ;
        return makeAff(function(k) {
          return map5(effectCanceler)(v.kill(e, k));
        });
      });
    };
  };
  var monadThrowAff = {
    throwError: _throwError,
    Monad0: function() {
      return monadAff;
    }
  };
  var monadErrorAff = {
    catchError: _catchError,
    MonadThrow0: function() {
      return monadThrowAff;
    }
  };
  var $$try2 = /* @__PURE__ */ $$try(monadErrorAff);
  var runAff = function(k) {
    return function(aff) {
      return launchAff(bindFlipped3(function($83) {
        return liftEffect2(k($83));
      })($$try2(aff)));
    };
  };
  var runAff_ = function(k) {
    return function(aff) {
      return $$void3(runAff(k)(aff));
    };
  };
  var monadRecAff = {
    tailRecM: function(k) {
      var go2 = function(a2) {
        return bind1(k(a2))(function(res) {
          if (res instanceof Done) {
            return pure22(res.value0);
          }
          ;
          if (res instanceof Loop) {
            return go2(res.value0);
          }
          ;
          throw new Error("Failed pattern match at Effect.Aff (line 104, column 7 - line 106, column 23): " + [res.constructor.name]);
        });
      };
      return go2;
    },
    Monad0: function() {
      return monadAff;
    }
  };
  var nonCanceler = /* @__PURE__ */ $$const(/* @__PURE__ */ pure22(unit));
  var monoidCanceler = {
    mempty: nonCanceler,
    Semigroup0: function() {
      return semigroupCanceler;
    }
  };

  // output/Effect.Aff.Class/index.js
  var lift4 = /* @__PURE__ */ lift(monadTransReaderT);
  var monadAffAff = {
    liftAff: /* @__PURE__ */ identity(categoryFn),
    MonadEffect0: function() {
      return monadEffectAff;
    }
  };
  var liftAff = function(dict) {
    return dict.liftAff;
  };
  var monadAffReader = function(dictMonadAff) {
    var MonadEffect0 = dictMonadAff.MonadEffect0();
    var monadEffectReader2 = monadEffectReader(MonadEffect0);
    return {
      liftAff: function() {
        var $79 = lift4(MonadEffect0.Monad0());
        var $80 = liftAff(dictMonadAff);
        return function($81) {
          return $79($80($81));
        };
      }(),
      MonadEffect0: function() {
        return monadEffectReader2;
      }
    };
  };

  // output/CSS.String/index.js
  var fromString = function(dict) {
    return dict.fromString;
  };

  // output/Data.Array/foreign.js
  var rangeImpl = function(start2, end) {
    var step3 = start2 > end ? -1 : 1;
    var result = new Array(step3 * (end - start2) + 1);
    var i2 = start2, n = 0;
    while (i2 !== end) {
      result[n++] = i2;
      i2 += step3;
    }
    result[n] = i2;
    return result;
  };
  var replicateFill = function(count, value14) {
    if (count < 1) {
      return [];
    }
    var result = new Array(count);
    return result.fill(value14);
  };
  var replicatePolyfill = function(count, value14) {
    var result = [];
    var n = 0;
    for (var i2 = 0; i2 < count; i2++) {
      result[n++] = value14;
    }
    return result;
  };
  var replicateImpl = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var fromFoldableImpl = /* @__PURE__ */ function() {
    function Cons2(head3, tail2) {
      this.head = head3;
      this.tail = tail2;
    }
    var emptyList = {};
    function curryCons(head3) {
      return function(tail2) {
        return new Cons2(head3, tail2);
      };
    }
    function listToArray(list) {
      var result = [];
      var count = 0;
      var xs = list;
      while (xs !== emptyList) {
        result[count++] = xs.head;
        xs = xs.tail;
      }
      return result;
    }
    return function(foldr4, xs) {
      return listToArray(foldr4(curryCons)(emptyList)(xs));
    };
  }();
  var length = function(xs) {
    return xs.length;
  };
  var findIndexImpl = function(just, nothing, f, xs) {
    for (var i2 = 0, l = xs.length; i2 < l; i2++) {
      if (f(xs[i2])) return just(i2);
    }
    return nothing;
  };
  var _deleteAt = function(just, nothing, i2, l) {
    if (i2 < 0 || i2 >= l.length) return nothing;
    var l1 = l.slice();
    l1.splice(i2, 1);
    return just(l1);
  };
  var zipWithImpl = function(f, xs, ys) {
    var l = xs.length < ys.length ? xs.length : ys.length;
    var result = new Array(l);
    for (var i2 = 0; i2 < l; i2++) {
      result[i2] = f(xs[i2])(ys[i2]);
    }
    return result;
  };

  // output/Data.Function.Uncurried/foreign.js
  var runFn2 = function(fn) {
    return function(a2) {
      return function(b2) {
        return fn(a2, b2);
      };
    };
  };
  var runFn3 = function(fn) {
    return function(a2) {
      return function(b2) {
        return function(c) {
          return fn(a2, b2, c);
        };
      };
    };
  };
  var runFn4 = function(fn) {
    return function(a2) {
      return function(b2) {
        return function(c) {
          return function(d) {
            return fn(a2, b2, c, d);
          };
        };
      };
    };
  };

  // output/Data.FunctorWithIndex/foreign.js
  var mapWithIndexArray = function(f) {
    return function(xs) {
      var l = xs.length;
      var result = Array(l);
      for (var i2 = 0; i2 < l; i2++) {
        result[i2] = f(i2)(xs[i2]);
      }
      return result;
    };
  };

  // output/Data.FunctorWithIndex/index.js
  var mapWithIndex = function(dict) {
    return dict.mapWithIndex;
  };
  var functorWithIndexArray = {
    mapWithIndex: mapWithIndexArray,
    Functor0: function() {
      return functorArray;
    }
  };

  // output/Data.Unfoldable/foreign.js
  var unfoldrArrayImpl = function(isNothing2) {
    return function(fromJust5) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b2) {
              var result = [];
              var value14 = b2;
              while (true) {
                var maybe2 = f(value14);
                if (isNothing2(maybe2)) return result;
                var tuple = fromJust5(maybe2);
                result.push(fst2(tuple));
                value14 = snd2(tuple);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/foreign.js
  var unfoldr1ArrayImpl = function(isNothing2) {
    return function(fromJust5) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b2) {
              var result = [];
              var value14 = b2;
              while (true) {
                var tuple = f(value14);
                result.push(fst2(tuple));
                var maybe2 = snd2(tuple);
                if (isNothing2(maybe2)) return result;
                value14 = fromJust5(maybe2);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/index.js
  var fromJust2 = /* @__PURE__ */ fromJust();
  var unfoldable1Array = {
    unfoldr1: /* @__PURE__ */ unfoldr1ArrayImpl(isNothing)(fromJust2)(fst)(snd)
  };

  // output/Data.Unfoldable/index.js
  var fromJust3 = /* @__PURE__ */ fromJust();
  var unfoldr = function(dict) {
    return dict.unfoldr;
  };
  var unfoldableArray = {
    unfoldr: /* @__PURE__ */ unfoldrArrayImpl(isNothing)(fromJust3)(fst)(snd),
    Unfoldable10: function() {
      return unfoldable1Array;
    }
  };

  // output/Data.Array/index.js
  var fromJust4 = /* @__PURE__ */ fromJust();
  var append2 = /* @__PURE__ */ append(semigroupArray);
  var zipWith = /* @__PURE__ */ runFn3(zipWithImpl);
  var zip = /* @__PURE__ */ function() {
    return zipWith(Tuple.create);
  }();
  var singleton2 = function(a2) {
    return [a2];
  };
  var range2 = /* @__PURE__ */ runFn2(rangeImpl);
  var fromFoldable = function(dictFoldable) {
    return runFn2(fromFoldableImpl)(foldr(dictFoldable));
  };
  var findIndex = /* @__PURE__ */ function() {
    return runFn4(findIndexImpl)(Just.create)(Nothing.value);
  }();
  var deleteAt = /* @__PURE__ */ function() {
    return runFn4(_deleteAt)(Just.create)(Nothing.value);
  }();
  var deleteBy = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2.length === 0) {
          return [];
        }
        ;
        return maybe(v2)(function(i2) {
          return fromJust4(deleteAt(i2)(v2));
        })(findIndex(v(v1))(v2));
      };
    };
  };
  var cons = function(x) {
    return function(xs) {
      return append2([x])(xs);
    };
  };
  var concatMap = /* @__PURE__ */ flip(/* @__PURE__ */ bind(bindArray));
  var mapMaybe = function(f) {
    return concatMap(function() {
      var $189 = maybe([])(singleton2);
      return function($190) {
        return $189(f($190));
      };
    }());
  };

  // output/Data.FoldableWithIndex/index.js
  var foldr8 = /* @__PURE__ */ foldr(foldableArray);
  var mapWithIndex2 = /* @__PURE__ */ mapWithIndex(functorWithIndexArray);
  var foldl8 = /* @__PURE__ */ foldl(foldableArray);
  var foldrWithIndex = function(dict) {
    return dict.foldrWithIndex;
  };
  var foldMapWithIndexDefaultR = function(dictFoldableWithIndex) {
    var foldrWithIndex1 = foldrWithIndex(dictFoldableWithIndex);
    return function(dictMonoid) {
      var append8 = append(dictMonoid.Semigroup0());
      var mempty3 = mempty(dictMonoid);
      return function(f) {
        return foldrWithIndex1(function(i2) {
          return function(x) {
            return function(acc) {
              return append8(f(i2)(x))(acc);
            };
          };
        })(mempty3);
      };
    };
  };
  var foldableWithIndexArray = {
    foldrWithIndex: function(f) {
      return function(z) {
        var $291 = foldr8(function(v) {
          return function(y) {
            return f(v.value0)(v.value1)(y);
          };
        })(z);
        var $292 = mapWithIndex2(Tuple.create);
        return function($293) {
          return $291($292($293));
        };
      };
    },
    foldlWithIndex: function(f) {
      return function(z) {
        var $294 = foldl8(function(y) {
          return function(v) {
            return f(v.value0)(y)(v.value1);
          };
        })(z);
        var $295 = mapWithIndex2(Tuple.create);
        return function($296) {
          return $294($295($296));
        };
      };
    },
    foldMapWithIndex: function(dictMonoid) {
      return foldMapWithIndexDefaultR(foldableWithIndexArray)(dictMonoid);
    },
    Foldable0: function() {
      return foldableArray;
    }
  };

  // output/Data.TraversableWithIndex/index.js
  var traverseWithIndexDefault = function(dictTraversableWithIndex) {
    var sequence2 = sequence(dictTraversableWithIndex.Traversable2());
    var mapWithIndex4 = mapWithIndex(dictTraversableWithIndex.FunctorWithIndex0());
    return function(dictApplicative) {
      var sequence12 = sequence2(dictApplicative);
      return function(f) {
        var $174 = mapWithIndex4(f);
        return function($175) {
          return sequence12($174($175));
        };
      };
    };
  };
  var traverseWithIndex = function(dict) {
    return dict.traverseWithIndex;
  };
  var traversableWithIndexArray = {
    traverseWithIndex: function(dictApplicative) {
      return traverseWithIndexDefault(traversableWithIndexArray)(dictApplicative);
    },
    FunctorWithIndex0: function() {
      return functorWithIndexArray;
    },
    FoldableWithIndex1: function() {
      return foldableWithIndexArray;
    },
    Traversable2: function() {
      return traversableArray;
    }
  };

  // output/Data.NonEmpty/index.js
  var NonEmpty = /* @__PURE__ */ function() {
    function NonEmpty2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    NonEmpty2.create = function(value0) {
      return function(value1) {
        return new NonEmpty2(value0, value1);
      };
    };
    return NonEmpty2;
  }();
  var singleton3 = function(dictPlus) {
    var empty7 = empty(dictPlus);
    return function(a2) {
      return new NonEmpty(a2, empty7);
    };
  };

  // output/Data.Int/foreign.js
  var fromNumberImpl = function(just) {
    return function(nothing) {
      return function(n) {
        return (n | 0) === n ? just(n) : nothing;
      };
    };
  };
  var toNumber = function(n) {
    return n;
  };
  var toStringAs = function(radix) {
    return function(i2) {
      return i2.toString(radix);
    };
  };

  // output/Data.Number/foreign.js
  var isFiniteImpl = isFinite;
  var abs = Math.abs;
  var remainder = function(n) {
    return function(m) {
      return n % m;
    };
  };
  var round = Math.round;

  // output/Data.Int/index.js
  var top2 = /* @__PURE__ */ top(boundedInt);
  var bottom2 = /* @__PURE__ */ bottom(boundedInt);
  var hexadecimal = 16;
  var fromNumber = /* @__PURE__ */ function() {
    return fromNumberImpl(Just.create)(Nothing.value);
  }();
  var unsafeClamp = function(x) {
    if (!isFiniteImpl(x)) {
      return 0;
    }
    ;
    if (x >= toNumber(top2)) {
      return top2;
    }
    ;
    if (x <= toNumber(bottom2)) {
      return bottom2;
    }
    ;
    if (otherwise) {
      return fromMaybe(0)(fromNumber(x));
    }
    ;
    throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): " + [x.constructor.name]);
  };
  var round2 = function($37) {
    return unsafeClamp(round($37));
  };

  // output/Data.String.CodePoints/foreign.js
  var hasArrayFrom = typeof Array.from === "function";
  var hasStringIterator = typeof Symbol !== "undefined" && Symbol != null && typeof Symbol.iterator !== "undefined" && typeof String.prototype[Symbol.iterator] === "function";
  var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
  var hasCodePointAt = typeof String.prototype.codePointAt === "function";
  var _unsafeCodePointAt0 = function(fallback) {
    return hasCodePointAt ? function(str) {
      return str.codePointAt(0);
    } : fallback;
  };
  var _toCodePointArray = function(fallback) {
    return function(unsafeCodePointAt02) {
      if (hasArrayFrom) {
        return function(str) {
          return Array.from(str, unsafeCodePointAt02);
        };
      }
      return fallback;
    };
  };

  // output/Data.Enum/foreign.js
  function toCharCode(c) {
    return c.charCodeAt(0);
  }
  function fromCharCode(c) {
    return String.fromCharCode(c);
  }

  // output/Data.Enum/index.js
  var bottom1 = /* @__PURE__ */ bottom(boundedChar);
  var top1 = /* @__PURE__ */ top(boundedChar);
  var fromEnum = function(dict) {
    return dict.fromEnum;
  };
  var defaultSucc = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a2) {
        return toEnum$prime(fromEnum$prime(a2) + 1 | 0);
      };
    };
  };
  var defaultPred = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a2) {
        return toEnum$prime(fromEnum$prime(a2) - 1 | 0);
      };
    };
  };
  var charToEnum = function(v) {
    if (v >= toCharCode(bottom1) && v <= toCharCode(top1)) {
      return new Just(fromCharCode(v));
    }
    ;
    return Nothing.value;
  };
  var enumChar = {
    succ: /* @__PURE__ */ defaultSucc(charToEnum)(toCharCode),
    pred: /* @__PURE__ */ defaultPred(charToEnum)(toCharCode),
    Ord0: function() {
      return ordChar;
    }
  };
  var boundedEnumChar = /* @__PURE__ */ function() {
    return {
      cardinality: toCharCode(top1) - toCharCode(bottom1) | 0,
      toEnum: charToEnum,
      fromEnum: toCharCode,
      Bounded0: function() {
        return boundedChar;
      },
      Enum1: function() {
        return enumChar;
      }
    };
  }();

  // output/Data.String.CodeUnits/foreign.js
  var length2 = function(s) {
    return s.length;
  };
  var drop2 = function(n) {
    return function(s) {
      return s.substring(n);
    };
  };

  // output/Data.String.Unsafe/foreign.js
  var charAt = function(i2) {
    return function(s) {
      if (i2 >= 0 && i2 < s.length) return s.charAt(i2);
      throw new Error("Data.String.Unsafe.charAt: Invalid index.");
    };
  };

  // output/Data.String.Common/foreign.js
  var joinWith = function(s) {
    return function(xs) {
      return xs.join(s);
    };
  };

  // output/Data.String.CodePoints/index.js
  var fromEnum2 = /* @__PURE__ */ fromEnum(boundedEnumChar);
  var map6 = /* @__PURE__ */ map(functorMaybe);
  var unfoldr2 = /* @__PURE__ */ unfoldr(unfoldableArray);
  var unsurrogate = function(lead) {
    return function(trail) {
      return (((lead - 55296 | 0) * 1024 | 0) + (trail - 56320 | 0) | 0) + 65536 | 0;
    };
  };
  var isTrail = function(cu) {
    return 56320 <= cu && cu <= 57343;
  };
  var isLead = function(cu) {
    return 55296 <= cu && cu <= 56319;
  };
  var uncons2 = function(s) {
    var v = length2(s);
    if (v === 0) {
      return Nothing.value;
    }
    ;
    if (v === 1) {
      return new Just({
        head: fromEnum2(charAt(0)(s)),
        tail: ""
      });
    }
    ;
    var cu1 = fromEnum2(charAt(1)(s));
    var cu0 = fromEnum2(charAt(0)(s));
    var $43 = isLead(cu0) && isTrail(cu1);
    if ($43) {
      return new Just({
        head: unsurrogate(cu0)(cu1),
        tail: drop2(2)(s)
      });
    }
    ;
    return new Just({
      head: cu0,
      tail: drop2(1)(s)
    });
  };
  var unconsButWithTuple = function(s) {
    return map6(function(v) {
      return new Tuple(v.head, v.tail);
    })(uncons2(s));
  };
  var toCodePointArrayFallback = function(s) {
    return unfoldr2(unconsButWithTuple)(s);
  };
  var unsafeCodePointAt0Fallback = function(s) {
    var cu0 = fromEnum2(charAt(0)(s));
    var $47 = isLead(cu0) && length2(s) > 1;
    if ($47) {
      var cu1 = fromEnum2(charAt(1)(s));
      var $48 = isTrail(cu1);
      if ($48) {
        return unsurrogate(cu0)(cu1);
      }
      ;
      return cu0;
    }
    ;
    return cu0;
  };
  var unsafeCodePointAt0 = /* @__PURE__ */ _unsafeCodePointAt0(unsafeCodePointAt0Fallback);
  var toCodePointArray = /* @__PURE__ */ _toCodePointArray(toCodePointArrayFallback)(unsafeCodePointAt0);
  var length3 = function($74) {
    return length(toCodePointArray($74));
  };

  // output/Color/index.js
  var clamp2 = /* @__PURE__ */ clamp(ordInt);
  var max3 = /* @__PURE__ */ max(ordInt);
  var min3 = /* @__PURE__ */ min(ordInt);
  var clamp1 = /* @__PURE__ */ clamp(ordNumber);
  var show2 = /* @__PURE__ */ show(showNumber);
  var HSLA = /* @__PURE__ */ function() {
    function HSLA2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    HSLA2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new HSLA2(value0, value1, value22, value32);
          };
        };
      };
    };
    return HSLA2;
  }();
  var modPos = function(x) {
    return function(y) {
      return remainder(remainder(x)(y) + y)(y);
    };
  };
  var rgba = function(red$prime) {
    return function(green$prime) {
      return function(blue$prime) {
        return function(alpha) {
          var red = clamp2(0)(255)(red$prime);
          var r = toNumber(red) / 255;
          var green = clamp2(0)(255)(green$prime);
          var g = toNumber(green) / 255;
          var blue = clamp2(0)(255)(blue$prime);
          var maxChroma = max3(max3(red)(green))(blue);
          var minChroma = min3(min3(red)(green))(blue);
          var chroma = maxChroma - minChroma | 0;
          var chroma$prime = toNumber(chroma) / 255;
          var lightness = toNumber(maxChroma + minChroma | 0) / (255 * 2);
          var saturation = function() {
            if (chroma === 0) {
              return 0;
            }
            ;
            if (otherwise) {
              return chroma$prime / (1 - abs(2 * lightness - 1));
            }
            ;
            throw new Error("Failed pattern match at Color (line 160, column 3 - line 162, column 64): ");
          }();
          var b2 = toNumber(blue) / 255;
          var hue$prime = function(v) {
            if (v === 0) {
              return 0;
            }
            ;
            if (maxChroma === red) {
              return modPos((g - b2) / chroma$prime)(6);
            }
            ;
            if (maxChroma === green) {
              return (b2 - r) / chroma$prime + 2;
            }
            ;
            if (otherwise) {
              return (r - g) / chroma$prime + 4;
            }
            ;
            throw new Error("Failed pattern match at Color (line 150, column 3 - line 150, column 15): " + [v.constructor.name]);
          };
          var hue = 60 * hue$prime(chroma);
          return new HSLA(hue, saturation, lightness, alpha);
        };
      };
    };
  };
  var rgb = function(r) {
    return function(g) {
      return function(b2) {
        return rgba(r)(g)(b2)(1);
      };
    };
  };
  var hsla = function(h) {
    return function(s) {
      return function(l) {
        return function(a2) {
          var s$prime = clamp1(0)(1)(s);
          var l$prime = clamp1(0)(1)(l);
          var a$prime = clamp1(0)(1)(a2);
          return new HSLA(h, s$prime, l$prime, a$prime);
        };
      };
    };
  };
  var hsl = function(h) {
    return function(s) {
      return function(l) {
        return hsla(h)(s)(l)(1);
      };
    };
  };
  var cssStringHSLA = function(v) {
    var toString = function(n) {
      return show2(toNumber(round2(100 * n)) / 100);
    };
    var saturation = toString(v.value1 * 100) + "%";
    var lightness = toString(v.value2 * 100) + "%";
    var hue = toString(v.value0);
    var alpha = show2(v.value3);
    var $118 = v.value3 === 1;
    if ($118) {
      return "hsl(" + (hue + (", " + (saturation + (", " + (lightness + ")")))));
    }
    ;
    return "hsla(" + (hue + (", " + (saturation + (", " + (lightness + (", " + (alpha + ")")))))));
  };
  var clipHue = function(v) {
    var $124 = 360 === v;
    if ($124) {
      return v;
    }
    ;
    return modPos(v)(360);
  };
  var toRGBA$prime = function(v) {
    var h$prime = clipHue(v.value0) / 60;
    var chr = (1 - abs(2 * v.value2 - 1)) * v.value1;
    var m = v.value2 - chr / 2;
    var x = chr * (1 - abs(remainder(h$prime)(2) - 1));
    var col2 = function() {
      if (h$prime < 1) {
        return {
          r: chr,
          g: x,
          b: 0
        };
      }
      ;
      if (1 <= h$prime && h$prime < 2) {
        return {
          r: x,
          g: chr,
          b: 0
        };
      }
      ;
      if (2 <= h$prime && h$prime < 3) {
        return {
          r: 0,
          g: chr,
          b: x
        };
      }
      ;
      if (3 <= h$prime && h$prime < 4) {
        return {
          r: 0,
          g: x,
          b: chr
        };
      }
      ;
      if (4 <= h$prime && h$prime < 5) {
        return {
          r: x,
          g: 0,
          b: chr
        };
      }
      ;
      if (otherwise) {
        return {
          r: chr,
          g: 0,
          b: x
        };
      }
      ;
      throw new Error("Failed pattern match at Color (line 356, column 3 - line 362, column 43): ");
    }();
    return {
      r: col2.r + m,
      g: col2.g + m,
      b: col2.b + m,
      a: v.value3
    };
  };
  var toRGBA = function(col2) {
    var c = toRGBA$prime(col2);
    var g = round2(255 * c.g);
    var r = round2(255 * c.r);
    var b2 = round2(255 * c.b);
    return {
      r,
      g,
      b: b2,
      a: c.a
    };
  };
  var toHexString = function(color) {
    var toHex = function(num) {
      var repr = toStringAs(hexadecimal)(num);
      var $152 = length3(repr) === 1;
      if ($152) {
        return "0" + repr;
      }
      ;
      return repr;
    };
    var c = toRGBA(color);
    var alpha = function() {
      if (c.a === 1) {
        return "";
      }
      ;
      if (otherwise) {
        return toHex(round2(255 * c.a));
      }
      ;
      throw new Error("Failed pattern match at Color (line 429, column 3 - line 431, column 46): ");
    }();
    return "#" + (toHex(c.r) + (toHex(c.g) + (toHex(c.b) + alpha)));
  };
  var black = /* @__PURE__ */ hsl(0)(0)(0);

  // output/Data.Profunctor.Strong/index.js
  var strongFn = {
    first: function(a2b) {
      return function(v) {
        return new Tuple(a2b(v.value0), v.value1);
      };
    },
    second: /* @__PURE__ */ map(functorTuple),
    Profunctor0: function() {
      return profunctorFn;
    }
  };
  var second = function(dict) {
    return dict.second;
  };

  // output/CSS.Property/index.js
  var map7 = /* @__PURE__ */ map(functorArray);
  var second2 = /* @__PURE__ */ second(strongFn);
  var append1 = /* @__PURE__ */ append(semigroupArray);
  var lookup2 = /* @__PURE__ */ lookup(foldableArray)(eqString);
  var Prefixed = /* @__PURE__ */ function() {
    function Prefixed2(value0) {
      this.value0 = value0;
    }
    ;
    Prefixed2.create = function(value0) {
      return new Prefixed2(value0);
    };
    return Prefixed2;
  }();
  var Plain = /* @__PURE__ */ function() {
    function Plain2(value0) {
      this.value0 = value0;
    }
    ;
    Plain2.create = function(value0) {
      return new Plain2(value0);
    };
    return Plain2;
  }();
  var Value = function(x) {
    return x;
  };
  var Key = function(x) {
    return x;
  };
  var value = function(dict) {
    return dict.value;
  };
  var semigroupPrefixed = {
    append: function(v) {
      return function(v1) {
        if (v instanceof Plain && v1 instanceof Plain) {
          return new Plain(v.value0 + v1.value0);
        }
        ;
        if (v instanceof Plain && v1 instanceof Prefixed) {
          return new Prefixed(map7(second2(function(v2) {
            return v.value0 + v2;
          }))(v1.value0));
        }
        ;
        if (v instanceof Prefixed && v1 instanceof Plain) {
          return new Prefixed(map7(second2(function(v2) {
            return v1.value0 + v2;
          }))(v.value0));
        }
        ;
        if (v instanceof Prefixed && v1 instanceof Prefixed) {
          return new Prefixed(append1(v.value0)(v1.value0));
        }
        ;
        throw new Error("Failed pattern match at CSS.Property (line 23, column 1 - line 27, column 59): " + [v.constructor.name, v1.constructor.name]);
      };
    }
  };
  var append22 = /* @__PURE__ */ append(semigroupPrefixed);
  var semigroupValue = {
    append: function(v) {
      return function(v1) {
        return append22(v)(v1);
      };
    }
  };
  var append3 = /* @__PURE__ */ append(semigroupValue);
  var plain = function(v) {
    if (v instanceof Prefixed) {
      return fromMaybe("")(lookup2("")(v.value0));
    }
    ;
    if (v instanceof Plain) {
      return v.value0;
    }
    ;
    throw new Error("Failed pattern match at CSS.Property (line 32, column 1 - line 32, column 28): " + [v.constructor.name]);
  };
  var isStringPrefixed = /* @__PURE__ */ function() {
    return {
      fromString: Plain.create
    };
  }();
  var fromString2 = /* @__PURE__ */ fromString(isStringPrefixed);
  var isStringValue = {
    fromString: function($141) {
      return Value(fromString2($141));
    }
  };
  var fromString1 = /* @__PURE__ */ fromString(isStringValue);
  var valColor = {
    value: function($144) {
      return fromString1(cssStringHSLA($144));
    }
  };
  var valNumber = {
    value: /* @__PURE__ */ function() {
      var $149 = show(showNumber);
      return function($150) {
        return fromString1($149($150));
      };
    }()
  };
  var valTuple = function(dictVal) {
    var value1 = value(dictVal);
    return function(dictVal1) {
      var value22 = value(dictVal1);
      return {
        value: function(v) {
          return append3(value1(v.value0))(append3(fromString1(" "))(value22(v.value1)));
        }
      };
    };
  };
  var isStringKey = {
    fromString: function($151) {
      return Key(fromString2($151));
    }
  };
  var cast = function(v) {
    return v;
  };

  // output/CSS.Common/index.js
  var browsers = /* @__PURE__ */ function() {
    return new Prefixed([new Tuple("-webkit-", ""), new Tuple("-moz-", ""), new Tuple("-ms-", ""), new Tuple("-o-", ""), new Tuple("", "")]);
  }();

  // output/Data.Exists/index.js
  var runExists = unsafeCoerce2;
  var mkExists = unsafeCoerce2;

  // output/CSS.Size/index.js
  var append4 = /* @__PURE__ */ append(semigroupValue);
  var value2 = /* @__PURE__ */ value(valNumber);
  var fromString3 = /* @__PURE__ */ fromString(isStringValue);
  var show3 = /* @__PURE__ */ show(showNumber);
  var append23 = /* @__PURE__ */ append(semigroupPrefixed);
  var BasicSize = /* @__PURE__ */ function() {
    function BasicSize2(value0) {
      this.value0 = value0;
    }
    ;
    BasicSize2.create = function(value0) {
      return new BasicSize2(value0);
    };
    return BasicSize2;
  }();
  var SumSize = /* @__PURE__ */ function() {
    function SumSize2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SumSize2.create = function(value0) {
      return function(value1) {
        return new SumSize2(value0, value1);
      };
    };
    return SumSize2;
  }();
  var DiffSize = /* @__PURE__ */ function() {
    function DiffSize2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    DiffSize2.create = function(value0) {
      return function(value1) {
        return new DiffSize2(value0, value1);
      };
    };
    return DiffSize2;
  }();
  var MultSize = /* @__PURE__ */ function() {
    function MultSize2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    MultSize2.create = function(value0) {
      return function(value1) {
        return new MultSize2(value0, value1);
      };
    };
    return MultSize2;
  }();
  var DivSize = /* @__PURE__ */ function() {
    function DivSize2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    DivSize2.create = function(value0) {
      return function(value1) {
        return new DivSize2(value0, value1);
      };
    };
    return DivSize2;
  }();
  var sizeToString = function(v) {
    if (v instanceof BasicSize) {
      return plain(v.value0);
    }
    ;
    if (v instanceof SumSize) {
      return runExists(function(a$prime) {
        return runExists(function(b$prime) {
          return "(" + (sizeToString(a$prime) + (" + " + (sizeToString(b$prime) + ")")));
        })(v.value1);
      })(v.value0);
    }
    ;
    if (v instanceof DiffSize) {
      return runExists(function(a$prime) {
        return runExists(function(b$prime) {
          return "(" + (sizeToString(a$prime) + (" - " + (sizeToString(b$prime) + ")")));
        })(v.value1);
      })(v.value0);
    }
    ;
    if (v instanceof MultSize) {
      return runExists(function(b$prime) {
        return "(" + (show3(v.value0) + (" * " + (sizeToString(b$prime) + ")")));
      })(v.value1);
    }
    ;
    if (v instanceof DivSize) {
      return runExists(function(b$prime) {
        return "(" + (sizeToString(b$prime) + (" / " + (show3(v.value0) + ")")));
      })(v.value1);
    }
    ;
    throw new Error("Failed pattern match at CSS.Size (line 29, column 1 - line 29, column 43): " + [v.constructor.name]);
  };
  var valSize = {
    value: function(v) {
      if (v instanceof BasicSize) {
        return v.value0;
      }
      ;
      return append23(browsers)(new Plain("calc" + sizeToString(v)));
    }
  };
  var px = function(i2) {
    return new BasicSize(append4(value2(i2))(fromString3("px")));
  };

  // output/Control.Monad.Writer/index.js
  var unwrap3 = /* @__PURE__ */ unwrap();
  var runWriter = function($5) {
    return unwrap3(runWriterT($5));
  };
  var execWriter = function(m) {
    return snd(runWriter(m));
  };

  // output/CSS.Stylesheet/index.js
  var map12 = /* @__PURE__ */ map(/* @__PURE__ */ functorWriterT(functorIdentity));
  var apply2 = /* @__PURE__ */ apply(/* @__PURE__ */ applyWriterT(semigroupArray)(applyIdentity));
  var bind2 = /* @__PURE__ */ bind(/* @__PURE__ */ bindWriterT(semigroupArray)(bindIdentity));
  var Property = /* @__PURE__ */ function() {
    function Property3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Property3.create = function(value0) {
      return function(value1) {
        return new Property3(value0, value1);
      };
    };
    return Property3;
  }();
  var S = function(x) {
    return x;
  };
  var runS = function(v) {
    return execWriter(v);
  };
  var rule = /* @__PURE__ */ function() {
    var $340 = tell(monadTellWriterT(monoidArray)(monadIdentity));
    return function($341) {
      return S($340(singleton2($341)));
    };
  }();
  var key = function(dictVal) {
    var value14 = value(dictVal);
    return function(k) {
      return function(v) {
        return rule(new Property(cast(k), value14(v)));
      };
    };
  };
  var functorStyleM = {
    map: function(f) {
      return function(v) {
        return map12(f)(v);
      };
    }
  };
  var applyStyleM = {
    apply: function(v) {
      return function(v1) {
        return apply2(v)(v1);
      };
    },
    Functor0: function() {
      return functorStyleM;
    }
  };
  var bindStyleM = {
    bind: function(v) {
      return function(f) {
        return bind2(v)(function($346) {
          return /* @__PURE__ */ function(v1) {
            return v1;
          }(f($346));
        });
      };
    },
    Apply0: function() {
      return applyStyleM;
    }
  };
  var applicativeStyleM = {
    pure: /* @__PURE__ */ function() {
      var $347 = pure(applicativeWriterT(monoidArray)(applicativeIdentity));
      return function($348) {
        return S($347($348));
      };
    }(),
    Apply0: function() {
      return applyStyleM;
    }
  };

  // output/CSS.Background/index.js
  var fromString12 = /* @__PURE__ */ fromString(isStringKey);
  var key2 = /* @__PURE__ */ key(valColor);
  var backgroundColor = /* @__PURE__ */ key2(/* @__PURE__ */ fromString12("background-color"));

  // output/CSS.Border/index.js
  var fromString4 = /* @__PURE__ */ fromString(isStringValue);
  var fromString13 = /* @__PURE__ */ fromString(isStringKey);
  var valTuple2 = /* @__PURE__ */ valTuple(valSize);
  var valStroke = {
    value: function(v) {
      return v;
    }
  };
  var key3 = /* @__PURE__ */ key(/* @__PURE__ */ valTuple(valStroke)(/* @__PURE__ */ valTuple2(valColor)));
  var solid = /* @__PURE__ */ fromString4("solid");
  var border = function(a2) {
    return function(b2) {
      return function(c) {
        return key3(fromString13("border"))(new Tuple(a2, new Tuple(b2, c)));
      };
    };
  };

  // output/CSS.Display/index.js
  var fromString5 = /* @__PURE__ */ fromString(isStringKey);
  var fromString14 = /* @__PURE__ */ fromString(isStringValue);
  var valDisplay = {
    value: function(v) {
      return v;
    }
  };
  var flex = /* @__PURE__ */ fromString14("flex");
  var display = /* @__PURE__ */ key(valDisplay)(/* @__PURE__ */ fromString5("display"));

  // output/CSS.Flexbox/index.js
  var fromString6 = /* @__PURE__ */ fromString(isStringValue);
  var fromString15 = /* @__PURE__ */ fromString(isStringKey);
  var valFlexDirection = {
    value: function(v) {
      return v;
    }
  };
  var row = /* @__PURE__ */ fromString6("row");
  var flexDirection = /* @__PURE__ */ key(valFlexDirection)(/* @__PURE__ */ fromString15("flex-direction"));

  // output/Data.List.Types/index.js
  var Nil = /* @__PURE__ */ function() {
    function Nil2() {
    }
    ;
    Nil2.value = new Nil2();
    return Nil2;
  }();
  var Cons = /* @__PURE__ */ function() {
    function Cons2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Cons2.create = function(value0) {
      return function(value1) {
        return new Cons2(value0, value1);
      };
    };
    return Cons2;
  }();
  var NonEmptyList = function(x) {
    return x;
  };
  var toList = function(v) {
    return new Cons(v.value0, v.value1);
  };
  var listMap = function(f) {
    var chunkedRevMap = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Cons)) {
            $tco_var_v = new Cons(v1, v);
            $copy_v1 = v1.value1.value1.value1;
            return;
          }
          ;
          var unrolledMap = function(v2) {
            if (v2 instanceof Cons && (v2.value1 instanceof Cons && v2.value1.value1 instanceof Nil)) {
              return new Cons(f(v2.value0), new Cons(f(v2.value1.value0), Nil.value));
            }
            ;
            if (v2 instanceof Cons && v2.value1 instanceof Nil) {
              return new Cons(f(v2.value0), Nil.value);
            }
            ;
            return Nil.value;
          };
          var reverseUnrolledMap = function($copy_v2) {
            return function($copy_v3) {
              var $tco_var_v2 = $copy_v2;
              var $tco_done1 = false;
              var $tco_result2;
              function $tco_loop2(v2, v3) {
                if (v2 instanceof Cons && (v2.value0 instanceof Cons && (v2.value0.value1 instanceof Cons && v2.value0.value1.value1 instanceof Cons))) {
                  $tco_var_v2 = v2.value1;
                  $copy_v3 = new Cons(f(v2.value0.value0), new Cons(f(v2.value0.value1.value0), new Cons(f(v2.value0.value1.value1.value0), v3)));
                  return;
                }
                ;
                $tco_done1 = true;
                return v3;
              }
              ;
              while (!$tco_done1) {
                $tco_result2 = $tco_loop2($tco_var_v2, $copy_v3);
              }
              ;
              return $tco_result2;
            };
          };
          $tco_done = true;
          return reverseUnrolledMap(v)(unrolledMap(v1));
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
    return chunkedRevMap(Nil.value);
  };
  var functorList = {
    map: listMap
  };
  var foldableList = {
    foldr: function(f) {
      return function(b2) {
        var rev3 = function() {
          var go2 = function($copy_v) {
            return function($copy_v1) {
              var $tco_var_v = $copy_v;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1) {
                if (v1 instanceof Nil) {
                  $tco_done = true;
                  return v;
                }
                ;
                if (v1 instanceof Cons) {
                  $tco_var_v = new Cons(v1.value0, v);
                  $copy_v1 = v1.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [v.constructor.name, v1.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $copy_v1);
              }
              ;
              return $tco_result;
            };
          };
          return go2(Nil.value);
        }();
        var $284 = foldl(foldableList)(flip(f))(b2);
        return function($285) {
          return $284(rev3($285));
        };
      };
    },
    foldl: function(f) {
      var go2 = function($copy_b) {
        return function($copy_v) {
          var $tco_var_b = $copy_b;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(b2, v) {
            if (v instanceof Nil) {
              $tco_done1 = true;
              return b2;
            }
            ;
            if (v instanceof Cons) {
              $tco_var_b = f(b2)(v.value0);
              $copy_v = v.value1;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_b, $copy_v);
          }
          ;
          return $tco_result;
        };
      };
      return go2;
    },
    foldMap: function(dictMonoid) {
      var append24 = append(dictMonoid.Semigroup0());
      var mempty3 = mempty(dictMonoid);
      return function(f) {
        return foldl(foldableList)(function(acc) {
          var $286 = append24(acc);
          return function($287) {
            return $286(f($287));
          };
        })(mempty3);
      };
    }
  };
  var foldr2 = /* @__PURE__ */ foldr(foldableList);
  var semigroupList = {
    append: function(xs) {
      return function(ys) {
        return foldr2(Cons.create)(ys)(xs);
      };
    }
  };
  var append12 = /* @__PURE__ */ append(semigroupList);
  var semigroupNonEmptyList = {
    append: function(v) {
      return function(as$prime) {
        return new NonEmpty(v.value0, append12(v.value1)(toList(as$prime)));
      };
    }
  };
  var altList = {
    alt: append12,
    Functor0: function() {
      return functorList;
    }
  };
  var plusList = /* @__PURE__ */ function() {
    return {
      empty: Nil.value,
      Alt0: function() {
        return altList;
      }
    };
  }();

  // output/Data.List/index.js
  var reverse2 = /* @__PURE__ */ function() {
    var go2 = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v1 instanceof Nil) {
            $tco_done = true;
            return v;
          }
          ;
          if (v1 instanceof Cons) {
            $tco_var_v = new Cons(v1.value0, v);
            $copy_v1 = v1.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [v.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
    return go2(Nil.value);
  }();
  var $$null = function(v) {
    if (v instanceof Nil) {
      return true;
    }
    ;
    return false;
  };

  // output/Data.CatQueue/index.js
  var CatQueue = /* @__PURE__ */ function() {
    function CatQueue2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CatQueue2.create = function(value0) {
      return function(value1) {
        return new CatQueue2(value0, value1);
      };
    };
    return CatQueue2;
  }();
  var uncons3 = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
        $tco_done = true;
        return Nothing.value;
      }
      ;
      if (v.value0 instanceof Nil) {
        $copy_v = new CatQueue(reverse2(v.value1), Nil.value);
        return;
      }
      ;
      if (v.value0 instanceof Cons) {
        $tco_done = true;
        return new Just(new Tuple(v.value0.value0, new CatQueue(v.value0.value1, v.value1)));
      }
      ;
      throw new Error("Failed pattern match at Data.CatQueue (line 82, column 1 - line 82, column 63): " + [v.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  var snoc2 = function(v) {
    return function(a2) {
      return new CatQueue(v.value0, new Cons(a2, v.value1));
    };
  };
  var $$null2 = function(v) {
    if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
      return true;
    }
    ;
    return false;
  };
  var empty2 = /* @__PURE__ */ function() {
    return new CatQueue(Nil.value, Nil.value);
  }();

  // output/Data.CatList/index.js
  var CatNil = /* @__PURE__ */ function() {
    function CatNil2() {
    }
    ;
    CatNil2.value = new CatNil2();
    return CatNil2;
  }();
  var CatCons = /* @__PURE__ */ function() {
    function CatCons2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CatCons2.create = function(value0) {
      return function(value1) {
        return new CatCons2(value0, value1);
      };
    };
    return CatCons2;
  }();
  var link = function(v) {
    return function(v1) {
      if (v instanceof CatNil) {
        return v1;
      }
      ;
      if (v1 instanceof CatNil) {
        return v;
      }
      ;
      if (v instanceof CatCons) {
        return new CatCons(v.value0, snoc2(v.value1)(v1));
      }
      ;
      throw new Error("Failed pattern match at Data.CatList (line 108, column 1 - line 108, column 54): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var foldr3 = function(k) {
    return function(b2) {
      return function(q2) {
        var foldl2 = function($copy_v) {
          return function($copy_v1) {
            return function($copy_v2) {
              var $tco_var_v = $copy_v;
              var $tco_var_v1 = $copy_v1;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1, v2) {
                if (v2 instanceof Nil) {
                  $tco_done = true;
                  return v1;
                }
                ;
                if (v2 instanceof Cons) {
                  $tco_var_v = v;
                  $tco_var_v1 = v(v1)(v2.value0);
                  $copy_v2 = v2.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.CatList (line 124, column 3 - line 124, column 59): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_v2);
              }
              ;
              return $tco_result;
            };
          };
        };
        var go2 = function($copy_xs) {
          return function($copy_ys) {
            var $tco_var_xs = $copy_xs;
            var $tco_done1 = false;
            var $tco_result;
            function $tco_loop(xs, ys) {
              var v = uncons3(xs);
              if (v instanceof Nothing) {
                $tco_done1 = true;
                return foldl2(function(x) {
                  return function(i2) {
                    return i2(x);
                  };
                })(b2)(ys);
              }
              ;
              if (v instanceof Just) {
                $tco_var_xs = v.value0.value1;
                $copy_ys = new Cons(k(v.value0.value0), ys);
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.CatList (line 120, column 14 - line 122, column 67): " + [v.constructor.name]);
            }
            ;
            while (!$tco_done1) {
              $tco_result = $tco_loop($tco_var_xs, $copy_ys);
            }
            ;
            return $tco_result;
          };
        };
        return go2(q2)(Nil.value);
      };
    };
  };
  var uncons4 = function(v) {
    if (v instanceof CatNil) {
      return Nothing.value;
    }
    ;
    if (v instanceof CatCons) {
      return new Just(new Tuple(v.value0, function() {
        var $66 = $$null2(v.value1);
        if ($66) {
          return CatNil.value;
        }
        ;
        return foldr3(link)(CatNil.value)(v.value1);
      }()));
    }
    ;
    throw new Error("Failed pattern match at Data.CatList (line 99, column 1 - line 99, column 61): " + [v.constructor.name]);
  };
  var empty3 = /* @__PURE__ */ function() {
    return CatNil.value;
  }();
  var append5 = link;
  var semigroupCatList = {
    append: append5
  };
  var snoc3 = function(cat) {
    return function(a2) {
      return append5(cat)(new CatCons(a2, empty2));
    };
  };

  // output/Control.Monad.Free/index.js
  var $runtime_lazy3 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var append6 = /* @__PURE__ */ append(semigroupCatList);
  var Free = /* @__PURE__ */ function() {
    function Free2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Free2.create = function(value0) {
      return function(value1) {
        return new Free2(value0, value1);
      };
    };
    return Free2;
  }();
  var Return = /* @__PURE__ */ function() {
    function Return2(value0) {
      this.value0 = value0;
    }
    ;
    Return2.create = function(value0) {
      return new Return2(value0);
    };
    return Return2;
  }();
  var Bind = /* @__PURE__ */ function() {
    function Bind2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Bind2.create = function(value0) {
      return function(value1) {
        return new Bind2(value0, value1);
      };
    };
    return Bind2;
  }();
  var toView = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      var runExpF = function(v22) {
        return v22;
      };
      var concatF = function(v22) {
        return function(r) {
          return new Free(v22.value0, append6(v22.value1)(r));
        };
      };
      if (v.value0 instanceof Return) {
        var v2 = uncons4(v.value1);
        if (v2 instanceof Nothing) {
          $tco_done = true;
          return new Return(v.value0.value0);
        }
        ;
        if (v2 instanceof Just) {
          $copy_v = concatF(runExpF(v2.value0.value0)(v.value0.value0))(v2.value0.value1);
          return;
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Free (line 227, column 7 - line 231, column 64): " + [v2.constructor.name]);
      }
      ;
      if (v.value0 instanceof Bind) {
        $tco_done = true;
        return new Bind(v.value0.value0, function(a2) {
          return concatF(v.value0.value1(a2))(v.value1);
        });
      }
      ;
      throw new Error("Failed pattern match at Control.Monad.Free (line 225, column 3 - line 233, column 56): " + [v.value0.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  var runFreeM = function(dictFunctor) {
    var map112 = map(dictFunctor);
    return function(dictMonadRec) {
      var Monad0 = dictMonadRec.Monad0();
      var map28 = map(Monad0.Bind1().Apply0().Functor0());
      var pure13 = pure(Monad0.Applicative0());
      var tailRecM4 = tailRecM(dictMonadRec);
      return function(k) {
        var go2 = function(f) {
          var v = toView(f);
          if (v instanceof Return) {
            return map28(Done.create)(pure13(v.value0));
          }
          ;
          if (v instanceof Bind) {
            return map28(Loop.create)(k(map112(v.value1)(v.value0)));
          }
          ;
          throw new Error("Failed pattern match at Control.Monad.Free (line 194, column 10 - line 196, column 37): " + [v.constructor.name]);
        };
        return tailRecM4(go2);
      };
    };
  };
  var fromView = function(f) {
    return new Free(f, empty3);
  };
  var freeMonad = {
    Applicative0: function() {
      return freeApplicative;
    },
    Bind1: function() {
      return freeBind;
    }
  };
  var freeFunctor = {
    map: function(k) {
      return function(f) {
        return bindFlipped(freeBind)(function() {
          var $189 = pure(freeApplicative);
          return function($190) {
            return $189(k($190));
          };
        }())(f);
      };
    }
  };
  var freeBind = {
    bind: function(v) {
      return function(k) {
        return new Free(v.value0, snoc3(v.value1)(k));
      };
    },
    Apply0: function() {
      return $lazy_freeApply(0);
    }
  };
  var freeApplicative = {
    pure: function($191) {
      return fromView(Return.create($191));
    },
    Apply0: function() {
      return $lazy_freeApply(0);
    }
  };
  var $lazy_freeApply = /* @__PURE__ */ $runtime_lazy3("freeApply", "Control.Monad.Free", function() {
    return {
      apply: ap(freeMonad),
      Functor0: function() {
        return freeFunctor;
      }
    };
  });
  var pure3 = /* @__PURE__ */ pure(freeApplicative);
  var liftF = function(f) {
    return fromView(new Bind(f, function($192) {
      return pure3($192);
    }));
  };
  var foldFree = function(dictMonadRec) {
    var Monad0 = dictMonadRec.Monad0();
    var map112 = map(Monad0.Bind1().Apply0().Functor0());
    var pure13 = pure(Monad0.Applicative0());
    var tailRecM4 = tailRecM(dictMonadRec);
    return function(k) {
      var go2 = function(f) {
        var v = toView(f);
        if (v instanceof Return) {
          return map112(Done.create)(pure13(v.value0));
        }
        ;
        if (v instanceof Bind) {
          return map112(function($199) {
            return Loop.create(v.value1($199));
          })(k(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Free (line 158, column 10 - line 160, column 37): " + [v.constructor.name]);
      };
      return tailRecM4(go2);
    };
  };

  // output/Graphics.Canvas/foreign.js
  function canvasElementToImageSource(e) {
    return e;
  }
  function getContext2D(c) {
    return function() {
      return c.getContext("2d");
    };
  }
  function getCanvasWidth(canvas2) {
    return function() {
      return canvas2.width;
    };
  }
  function getCanvasHeight(canvas2) {
    return function() {
      return canvas2.height;
    };
  }
  function setCanvasWidth(canvas2) {
    return function(width9) {
      return function() {
        canvas2.width = width9;
      };
    };
  }
  function setCanvasHeight(canvas2) {
    return function(height9) {
      return function() {
        canvas2.height = height9;
      };
    };
  }
  function canvasToDataURL(canvas2) {
    return function() {
      return canvas2.toDataURL();
    };
  }
  function setLineWidth(ctx) {
    return function(width9) {
      return function() {
        ctx.lineWidth = width9;
      };
    };
  }
  function setLineDash(ctx) {
    return function(dash) {
      return function() {
        ctx.setLineDash(dash);
      };
    };
  }
  function setFillStyle(ctx) {
    return function(style3) {
      return function() {
        ctx.fillStyle = style3;
      };
    };
  }
  function setStrokeStyle(ctx) {
    return function(style3) {
      return function() {
        ctx.strokeStyle = style3;
      };
    };
  }
  function setShadowColor(ctx) {
    return function(color) {
      return function() {
        ctx.shadowColor = color;
      };
    };
  }
  function setShadowBlur(ctx) {
    return function(blur3) {
      return function() {
        ctx.shadowBlur = blur3;
      };
    };
  }
  function setShadowOffsetX(ctx) {
    return function(offsetX) {
      return function() {
        ctx.shadowOffsetX = offsetX;
      };
    };
  }
  function setShadowOffsetY(ctx) {
    return function(offsetY) {
      return function() {
        ctx.shadowOffsetY = offsetY;
      };
    };
  }
  function setMiterLimit(ctx) {
    return function(limit) {
      return function() {
        ctx.miterLimit = limit;
      };
    };
  }
  function setLineCapImpl(ctx) {
    return function(cap) {
      return function() {
        ctx.lineCap = cap;
      };
    };
  }
  function setLineJoinImpl(ctx) {
    return function(join3) {
      return function() {
        ctx.lineJoin = join3;
      };
    };
  }
  function setGlobalCompositeOperationImpl(ctx) {
    return function(op) {
      return function() {
        ctx.globalCompositeOperation = op;
      };
    };
  }
  function setGlobalAlpha(ctx) {
    return function(alpha) {
      return function() {
        ctx.globalAlpha = alpha;
      };
    };
  }
  function beginPath(ctx) {
    return function() {
      ctx.beginPath();
    };
  }
  function stroke(ctx) {
    return function() {
      ctx.stroke();
    };
  }
  function fill(ctx) {
    return function() {
      ctx.fill();
    };
  }
  function clip(ctx) {
    return function() {
      ctx.clip();
    };
  }
  function lineTo(ctx) {
    return function(x) {
      return function(y) {
        return function() {
          ctx.lineTo(x, y);
        };
      };
    };
  }
  function moveTo(ctx) {
    return function(x) {
      return function(y) {
        return function() {
          ctx.moveTo(x, y);
        };
      };
    };
  }
  function closePath(ctx) {
    return function() {
      ctx.closePath();
    };
  }
  function arc(ctx) {
    return function(a2) {
      return function() {
        ctx.arc(a2.x, a2.y, a2.radius, a2.start, a2.end, a2.useCounterClockwise);
      };
    };
  }
  function rect(ctx) {
    return function(r) {
      return function() {
        ctx.rect(r.x, r.y, r.width, r.height);
      };
    };
  }
  function fillRect(ctx) {
    return function(r) {
      return function() {
        ctx.fillRect(r.x, r.y, r.width, r.height);
      };
    };
  }
  function strokeRect(ctx) {
    return function(r) {
      return function() {
        ctx.strokeRect(r.x, r.y, r.width, r.height);
      };
    };
  }
  function clearRect(ctx) {
    return function(r) {
      return function() {
        ctx.clearRect(r.x, r.y, r.width, r.height);
      };
    };
  }
  function scale(ctx) {
    return function(t) {
      return function() {
        ctx.scale(t.scaleX, t.scaleY);
      };
    };
  }
  function rotate(ctx) {
    return function(angle) {
      return function() {
        ctx.rotate(angle);
      };
    };
  }
  function translate(ctx) {
    return function(t) {
      return function() {
        ctx.translate(t.translateX, t.translateY);
      };
    };
  }
  function transform(ctx) {
    return function(t) {
      return function() {
        ctx.transform(t.a, t.b, t.c, t.d, t.e, t.f);
      };
    };
  }
  function setTransform(ctx) {
    return function(t) {
      return function() {
        ctx.setTransform(t.a, t.b, t.c, t.d, t.e, t.f);
      };
    };
  }
  function textAlignImpl(ctx) {
    return function() {
      return ctx.textAlign;
    };
  }
  function setTextAlignImpl(ctx) {
    return function(textAlign2) {
      return function() {
        ctx.textAlign = textAlign2;
      };
    };
  }
  function textBaselineImpl(ctx) {
    return function() {
      return ctx.textBaseline;
    };
  }
  function setTextBaselineImpl(ctx) {
    return function(textBaseline2) {
      return function() {
        ctx.textBaseline = textBaseline2;
      };
    };
  }
  function font(ctx) {
    return function() {
      return ctx.font;
    };
  }
  function setFont(ctx) {
    return function(fontspec) {
      return function() {
        ctx.font = fontspec;
      };
    };
  }
  function fillText(ctx) {
    return function(text6) {
      return function(x) {
        return function(y) {
          return function() {
            ctx.fillText(text6, x, y);
          };
        };
      };
    };
  }
  function strokeText(ctx) {
    return function(text6) {
      return function(x) {
        return function(y) {
          return function() {
            ctx.strokeText(text6, x, y);
          };
        };
      };
    };
  }
  function measureText(ctx) {
    return function(text6) {
      return function() {
        return ctx.measureText(text6);
      };
    };
  }
  function save(ctx) {
    return function() {
      ctx.save();
    };
  }
  function restore(ctx) {
    return function() {
      ctx.restore();
    };
  }
  function getImageData(ctx) {
    return function(x) {
      return function(y) {
        return function(w) {
          return function(h) {
            return function() {
              return ctx.getImageData(x, y, w, h);
            };
          };
        };
      };
    };
  }
  function putImageDataFull(ctx) {
    return function(image_data) {
      return function(x) {
        return function(y) {
          return function(dx) {
            return function(dy) {
              return function(dw) {
                return function(dh) {
                  return function() {
                    ctx.putImageData(image_data, x, y, dx, dy, dw, dh);
                  };
                };
              };
            };
          };
        };
      };
    };
  }
  function putImageData(ctx) {
    return function(image_data) {
      return function(x) {
        return function(y) {
          return function() {
            ctx.putImageData(image_data, x, y);
          };
        };
      };
    };
  }
  function createImageData(ctx) {
    return function(sw) {
      return function(sh) {
        return function() {
          return ctx.createImageData(sw, sh);
        };
      };
    };
  }
  function drawImage(ctx) {
    return function(image_source) {
      return function(dx) {
        return function(dy) {
          return function() {
            ctx.drawImage(image_source, dx, dy);
          };
        };
      };
    };
  }
  function drawImageScale(ctx) {
    return function(image_source) {
      return function(dx) {
        return function(dy) {
          return function(dWidth) {
            return function(dHeight) {
              return function() {
                ctx.drawImage(image_source, dx, dy, dWidth, dHeight);
              };
            };
          };
        };
      };
    };
  }
  function drawImageFull(ctx) {
    return function(image_source) {
      return function(sx) {
        return function(sy) {
          return function(sWidth) {
            return function(sHeight) {
              return function(dx) {
                return function(dy) {
                  return function(dWidth) {
                    return function(dHeight) {
                      return function() {
                        ctx.drawImage(image_source, sx, sy, sWidth, sHeight, dx, dy, dWidth, dHeight);
                      };
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  }
  function createPatternImpl(ctx) {
    return function(img2) {
      return function(repeat2) {
        return function() {
          return ctx.createPattern(img2, repeat2);
        };
      };
    };
  }
  function setPatternFillStyle(ctx) {
    return function(pattern2) {
      return function() {
        ctx.fillStyle = pattern2;
      };
    };
  }
  function createLinearGradient(ctx) {
    return function(linearGradient) {
      return function() {
        return ctx.createLinearGradient(linearGradient.x0, linearGradient.y0, linearGradient.x1, linearGradient.y1);
      };
    };
  }
  function createRadialGradient(ctx) {
    return function(radialGradient) {
      return function() {
        return ctx.createRadialGradient(radialGradient.x0, radialGradient.y0, radialGradient.r0, radialGradient.x1, radialGradient.y1, radialGradient.r1);
      };
    };
  }
  function setGradientFillStyle(ctx) {
    return function(gradient) {
      return function() {
        ctx.fillStyle = gradient;
      };
    };
  }
  function quadraticCurveTo(ctx) {
    return function(qCurve) {
      return function() {
        ctx.quadraticCurveTo(qCurve.cpx, qCurve.cpy, qCurve.x, qCurve.y);
      };
    };
  }
  function bezierCurveTo(ctx) {
    return function(bCurve) {
      return function() {
        ctx.bezierCurveTo(bCurve.cp1x, bCurve.cp1y, bCurve.cp2x, bCurve.cp2y, bCurve.x, bCurve.y);
      };
    };
  }

  // output/Effect.Exception.Unsafe/index.js
  var unsafeThrowException = function($1) {
    return unsafePerformEffect(throwException($1));
  };
  var unsafeThrow = function($2) {
    return unsafeThrowException(error($2));
  };

  // output/Graphics.Canvas/index.js
  var map8 = /* @__PURE__ */ map(functorEffect);
  var applySecond2 = /* @__PURE__ */ applySecond(applyEffect);
  var BaselineTop = /* @__PURE__ */ function() {
    function BaselineTop2() {
    }
    ;
    BaselineTop2.value = new BaselineTop2();
    return BaselineTop2;
  }();
  var BaselineHanging = /* @__PURE__ */ function() {
    function BaselineHanging2() {
    }
    ;
    BaselineHanging2.value = new BaselineHanging2();
    return BaselineHanging2;
  }();
  var BaselineMiddle = /* @__PURE__ */ function() {
    function BaselineMiddle2() {
    }
    ;
    BaselineMiddle2.value = new BaselineMiddle2();
    return BaselineMiddle2;
  }();
  var BaselineAlphabetic = /* @__PURE__ */ function() {
    function BaselineAlphabetic2() {
    }
    ;
    BaselineAlphabetic2.value = new BaselineAlphabetic2();
    return BaselineAlphabetic2;
  }();
  var BaselineIdeographic = /* @__PURE__ */ function() {
    function BaselineIdeographic2() {
    }
    ;
    BaselineIdeographic2.value = new BaselineIdeographic2();
    return BaselineIdeographic2;
  }();
  var BaselineBottom = /* @__PURE__ */ function() {
    function BaselineBottom2() {
    }
    ;
    BaselineBottom2.value = new BaselineBottom2();
    return BaselineBottom2;
  }();
  var AlignLeft = /* @__PURE__ */ function() {
    function AlignLeft2() {
    }
    ;
    AlignLeft2.value = new AlignLeft2();
    return AlignLeft2;
  }();
  var AlignRight = /* @__PURE__ */ function() {
    function AlignRight2() {
    }
    ;
    AlignRight2.value = new AlignRight2();
    return AlignRight2;
  }();
  var AlignCenter = /* @__PURE__ */ function() {
    function AlignCenter2() {
    }
    ;
    AlignCenter2.value = new AlignCenter2();
    return AlignCenter2;
  }();
  var AlignStart = /* @__PURE__ */ function() {
    function AlignStart2() {
    }
    ;
    AlignStart2.value = new AlignStart2();
    return AlignStart2;
  }();
  var AlignEnd = /* @__PURE__ */ function() {
    function AlignEnd2() {
    }
    ;
    AlignEnd2.value = new AlignEnd2();
    return AlignEnd2;
  }();
  var Repeat = /* @__PURE__ */ function() {
    function Repeat2() {
    }
    ;
    Repeat2.value = new Repeat2();
    return Repeat2;
  }();
  var RepeatX = /* @__PURE__ */ function() {
    function RepeatX2() {
    }
    ;
    RepeatX2.value = new RepeatX2();
    return RepeatX2;
  }();
  var RepeatY = /* @__PURE__ */ function() {
    function RepeatY2() {
    }
    ;
    RepeatY2.value = new RepeatY2();
    return RepeatY2;
  }();
  var NoRepeat = /* @__PURE__ */ function() {
    function NoRepeat2() {
    }
    ;
    NoRepeat2.value = new NoRepeat2();
    return NoRepeat2;
  }();
  var BevelJoin = /* @__PURE__ */ function() {
    function BevelJoin2() {
    }
    ;
    BevelJoin2.value = new BevelJoin2();
    return BevelJoin2;
  }();
  var RoundJoin = /* @__PURE__ */ function() {
    function RoundJoin2() {
    }
    ;
    RoundJoin2.value = new RoundJoin2();
    return RoundJoin2;
  }();
  var MiterJoin = /* @__PURE__ */ function() {
    function MiterJoin2() {
    }
    ;
    MiterJoin2.value = new MiterJoin2();
    return MiterJoin2;
  }();
  var Round = /* @__PURE__ */ function() {
    function Round2() {
    }
    ;
    Round2.value = new Round2();
    return Round2;
  }();
  var Square = /* @__PURE__ */ function() {
    function Square2() {
    }
    ;
    Square2.value = new Square2();
    return Square2;
  }();
  var Butt = /* @__PURE__ */ function() {
    function Butt2() {
    }
    ;
    Butt2.value = new Butt2();
    return Butt2;
  }();
  var SourceOver = /* @__PURE__ */ function() {
    function SourceOver2() {
    }
    ;
    SourceOver2.value = new SourceOver2();
    return SourceOver2;
  }();
  var SourceIn = /* @__PURE__ */ function() {
    function SourceIn2() {
    }
    ;
    SourceIn2.value = new SourceIn2();
    return SourceIn2;
  }();
  var SourceOut = /* @__PURE__ */ function() {
    function SourceOut2() {
    }
    ;
    SourceOut2.value = new SourceOut2();
    return SourceOut2;
  }();
  var SourceAtop = /* @__PURE__ */ function() {
    function SourceAtop2() {
    }
    ;
    SourceAtop2.value = new SourceAtop2();
    return SourceAtop2;
  }();
  var DestinationOver = /* @__PURE__ */ function() {
    function DestinationOver2() {
    }
    ;
    DestinationOver2.value = new DestinationOver2();
    return DestinationOver2;
  }();
  var DestinationIn = /* @__PURE__ */ function() {
    function DestinationIn2() {
    }
    ;
    DestinationIn2.value = new DestinationIn2();
    return DestinationIn2;
  }();
  var DestinationOut = /* @__PURE__ */ function() {
    function DestinationOut2() {
    }
    ;
    DestinationOut2.value = new DestinationOut2();
    return DestinationOut2;
  }();
  var DestinationAtop = /* @__PURE__ */ function() {
    function DestinationAtop2() {
    }
    ;
    DestinationAtop2.value = new DestinationAtop2();
    return DestinationAtop2;
  }();
  var Lighter = /* @__PURE__ */ function() {
    function Lighter2() {
    }
    ;
    Lighter2.value = new Lighter2();
    return Lighter2;
  }();
  var Copy = /* @__PURE__ */ function() {
    function Copy2() {
    }
    ;
    Copy2.value = new Copy2();
    return Copy2;
  }();
  var Xor = /* @__PURE__ */ function() {
    function Xor2() {
    }
    ;
    Xor2.value = new Xor2();
    return Xor2;
  }();
  var Multiply = /* @__PURE__ */ function() {
    function Multiply2() {
    }
    ;
    Multiply2.value = new Multiply2();
    return Multiply2;
  }();
  var Screen = /* @__PURE__ */ function() {
    function Screen2() {
    }
    ;
    Screen2.value = new Screen2();
    return Screen2;
  }();
  var Overlay = /* @__PURE__ */ function() {
    function Overlay2() {
    }
    ;
    Overlay2.value = new Overlay2();
    return Overlay2;
  }();
  var Darken = /* @__PURE__ */ function() {
    function Darken2() {
    }
    ;
    Darken2.value = new Darken2();
    return Darken2;
  }();
  var Lighten = /* @__PURE__ */ function() {
    function Lighten2() {
    }
    ;
    Lighten2.value = new Lighten2();
    return Lighten2;
  }();
  var ColorDodge = /* @__PURE__ */ function() {
    function ColorDodge2() {
    }
    ;
    ColorDodge2.value = new ColorDodge2();
    return ColorDodge2;
  }();
  var ColorBurn = /* @__PURE__ */ function() {
    function ColorBurn2() {
    }
    ;
    ColorBurn2.value = new ColorBurn2();
    return ColorBurn2;
  }();
  var HardLight = /* @__PURE__ */ function() {
    function HardLight2() {
    }
    ;
    HardLight2.value = new HardLight2();
    return HardLight2;
  }();
  var SoftLight = /* @__PURE__ */ function() {
    function SoftLight2() {
    }
    ;
    SoftLight2.value = new SoftLight2();
    return SoftLight2;
  }();
  var Difference = /* @__PURE__ */ function() {
    function Difference2() {
    }
    ;
    Difference2.value = new Difference2();
    return Difference2;
  }();
  var Exclusion = /* @__PURE__ */ function() {
    function Exclusion2() {
    }
    ;
    Exclusion2.value = new Exclusion2();
    return Exclusion2;
  }();
  var Hue = /* @__PURE__ */ function() {
    function Hue2() {
    }
    ;
    Hue2.value = new Hue2();
    return Hue2;
  }();
  var Saturation = /* @__PURE__ */ function() {
    function Saturation2() {
    }
    ;
    Saturation2.value = new Saturation2();
    return Saturation2;
  }();
  var Color = /* @__PURE__ */ function() {
    function Color2() {
    }
    ;
    Color2.value = new Color2();
    return Color2;
  }();
  var Luminosity = /* @__PURE__ */ function() {
    function Luminosity2() {
    }
    ;
    Luminosity2.value = new Luminosity2();
    return Luminosity2;
  }();
  var textBaseline = function(ctx) {
    var unsafeParseTextBaseline = function(v) {
      if (v === "top") {
        return BaselineTop.value;
      }
      ;
      if (v === "hanging") {
        return BaselineHanging.value;
      }
      ;
      if (v === "middle") {
        return BaselineMiddle.value;
      }
      ;
      if (v === "alphabetic") {
        return BaselineAlphabetic.value;
      }
      ;
      if (v === "ideographic") {
        return BaselineIdeographic.value;
      }
      ;
      if (v === "bottom") {
        return BaselineBottom.value;
      }
      ;
      return unsafeThrow("invalid TextBaseline: " + v);
    };
    return map8(unsafeParseTextBaseline)(textBaselineImpl(ctx));
  };
  var textAlign = function(ctx) {
    var unsafeParseTextAlign = function(v) {
      if (v === "left") {
        return AlignLeft.value;
      }
      ;
      if (v === "right") {
        return AlignRight.value;
      }
      ;
      if (v === "center") {
        return AlignCenter.value;
      }
      ;
      if (v === "start") {
        return AlignStart.value;
      }
      ;
      if (v === "end") {
        return AlignEnd.value;
      }
      ;
      return unsafeThrow("invalid TextAlign: " + v);
    };
    return map8(unsafeParseTextAlign)(textAlignImpl(ctx));
  };
  var setTextBaseline = function(ctx) {
    return function(textbaseline) {
      var toString = function(v) {
        if (v instanceof BaselineTop) {
          return "top";
        }
        ;
        if (v instanceof BaselineHanging) {
          return "hanging";
        }
        ;
        if (v instanceof BaselineMiddle) {
          return "middle";
        }
        ;
        if (v instanceof BaselineAlphabetic) {
          return "alphabetic";
        }
        ;
        if (v instanceof BaselineIdeographic) {
          return "ideographic";
        }
        ;
        if (v instanceof BaselineBottom) {
          return "bottom";
        }
        ;
        throw new Error("Failed pattern match at Graphics.Canvas (line 577, column 5 - line 577, column 33): " + [v.constructor.name]);
      };
      return setTextBaselineImpl(ctx)(toString(textbaseline));
    };
  };
  var setTextAlign = function(ctx) {
    return function(textalign) {
      var toString = function(v) {
        if (v instanceof AlignLeft) {
          return "left";
        }
        ;
        if (v instanceof AlignRight) {
          return "right";
        }
        ;
        if (v instanceof AlignCenter) {
          return "center";
        }
        ;
        if (v instanceof AlignStart) {
          return "start";
        }
        ;
        if (v instanceof AlignEnd) {
          return "end";
        }
        ;
        throw new Error("Failed pattern match at Graphics.Canvas (line 531, column 5 - line 531, column 32): " + [v.constructor.name]);
      };
      return setTextAlignImpl(ctx)(toString(textalign));
    };
  };
  var setLineJoin = function(v) {
    return function(v1) {
      if (v1 instanceof BevelJoin) {
        return setLineJoinImpl(v)("bevel");
      }
      ;
      if (v1 instanceof RoundJoin) {
        return setLineJoinImpl(v)("round");
      }
      ;
      if (v1 instanceof MiterJoin) {
        return setLineJoinImpl(v)("miter");
      }
      ;
      throw new Error("Failed pattern match at Graphics.Canvas (line 253, column 1 - line 253, column 52): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var setLineCap = function(v) {
    return function(v1) {
      if (v1 instanceof Round) {
        return setLineCapImpl(v)("round");
      }
      ;
      if (v1 instanceof Square) {
        return setLineCapImpl(v)("square");
      }
      ;
      if (v1 instanceof Butt) {
        return setLineCapImpl(v)("butt");
      }
      ;
      throw new Error("Failed pattern match at Graphics.Canvas (line 238, column 1 - line 238, column 50): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var setGlobalCompositeOperation = function(ctx) {
    return function(composite) {
      var toString = function(v) {
        if (v instanceof SourceOver) {
          return "source-over";
        }
        ;
        if (v instanceof SourceIn) {
          return "source-in";
        }
        ;
        if (v instanceof SourceOut) {
          return "source-out";
        }
        ;
        if (v instanceof SourceAtop) {
          return "source-atop";
        }
        ;
        if (v instanceof DestinationOver) {
          return "destination-over";
        }
        ;
        if (v instanceof DestinationIn) {
          return "destination-in";
        }
        ;
        if (v instanceof DestinationOut) {
          return "destination-out";
        }
        ;
        if (v instanceof DestinationAtop) {
          return "destination-atop";
        }
        ;
        if (v instanceof Lighter) {
          return "lighter";
        }
        ;
        if (v instanceof Copy) {
          return "copy";
        }
        ;
        if (v instanceof Xor) {
          return "xor";
        }
        ;
        if (v instanceof Multiply) {
          return "multiply";
        }
        ;
        if (v instanceof Screen) {
          return "screen";
        }
        ;
        if (v instanceof Overlay) {
          return "overlay";
        }
        ;
        if (v instanceof Darken) {
          return "darken";
        }
        ;
        if (v instanceof Lighten) {
          return "lighten";
        }
        ;
        if (v instanceof ColorDodge) {
          return "color-dodge";
        }
        ;
        if (v instanceof ColorBurn) {
          return "color-burn";
        }
        ;
        if (v instanceof HardLight) {
          return "hard-light";
        }
        ;
        if (v instanceof SoftLight) {
          return "soft-light";
        }
        ;
        if (v instanceof Difference) {
          return "difference";
        }
        ;
        if (v instanceof Exclusion) {
          return "exclusion";
        }
        ;
        if (v instanceof Hue) {
          return "hue";
        }
        ;
        if (v instanceof Saturation) {
          return "saturation";
        }
        ;
        if (v instanceof Color) {
          return "color";
        }
        ;
        if (v instanceof Luminosity) {
          return "luminosity";
        }
        ;
        throw new Error("Failed pattern match at Graphics.Canvas (line 326, column 5 - line 326, column 45): " + [v.constructor.name]);
      };
      return setGlobalCompositeOperationImpl(ctx)(toString(composite));
    };
  };
  var setCanvasDimensions = function(ce) {
    return function(d) {
      return applySecond2(setCanvasHeight(ce)(d.height))(setCanvasWidth(ce)(d.width));
    };
  };
  var getCanvasDimensions = function(ce) {
    return function __do2() {
      var w = getCanvasWidth(ce)();
      var h = getCanvasHeight(ce)();
      return {
        width: w,
        height: h
      };
    };
  };
  var createPattern = function(context) {
    return function(img2) {
      return function(repeat2) {
        var toString = function(v) {
          if (v instanceof Repeat) {
            return "repeat";
          }
          ;
          if (v instanceof RepeatX) {
            return "repeat-x";
          }
          ;
          if (v instanceof RepeatY) {
            return "repeat-y";
          }
          ;
          if (v instanceof NoRepeat) {
            return "no-repeat";
          }
          ;
          throw new Error("Failed pattern match at Graphics.Canvas (line 677, column 5 - line 677, column 31): " + [v.constructor.name]);
        };
        return createPatternImpl(context)(img2)(toString(repeat2));
      };
    };
  };

  // output/Graphics.Canvas.Extra/foreign.js
  var _convertToJpegBlob = (canvas2) => (quality) => () => {
    return canvas2.convertToBlob({ type: "image/jpeg", quality });
  };

  // output/Control.Promise/foreign.js
  function thenImpl(promise2) {
    return function(errCB) {
      return function(succCB) {
        return function() {
          promise2.then(succCB, errCB);
        };
      };
    };
  }

  // output/Control.Monad.Except/index.js
  var unwrap4 = /* @__PURE__ */ unwrap();
  var runExcept = function($3) {
    return unwrap4(runExceptT($3));
  };

  // output/Foreign/foreign.js
  function typeOf(value14) {
    return typeof value14;
  }
  function tagOf(value14) {
    return Object.prototype.toString.call(value14).slice(8, -1);
  }
  var isArray = Array.isArray || function(value14) {
    return Object.prototype.toString.call(value14) === "[object Array]";
  };

  // output/Data.List.NonEmpty/index.js
  var singleton5 = /* @__PURE__ */ function() {
    var $200 = singleton3(plusList);
    return function($201) {
      return NonEmptyList($200($201));
    };
  }();
  var cons2 = function(y) {
    return function(v) {
      return new NonEmpty(y, new Cons(v.value0, v.value1));
    };
  };

  // output/Foreign/index.js
  var TypeMismatch = /* @__PURE__ */ function() {
    function TypeMismatch2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TypeMismatch2.create = function(value0) {
      return function(value1) {
        return new TypeMismatch2(value0, value1);
      };
    };
    return TypeMismatch2;
  }();
  var unsafeFromForeign = unsafeCoerce2;
  var fail = function(dictMonad) {
    var $153 = throwError(monadThrowExceptT(dictMonad));
    return function($154) {
      return $153(singleton5($154));
    };
  };
  var unsafeReadTagged = function(dictMonad) {
    var pure13 = pure(applicativeExceptT(dictMonad));
    var fail1 = fail(dictMonad);
    return function(tag) {
      return function(value14) {
        if (tagOf(value14) === tag) {
          return pure13(unsafeFromForeign(value14));
        }
        ;
        if (otherwise) {
          return fail1(new TypeMismatch(tag, tagOf(value14)));
        }
        ;
        throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): " + [tag.constructor.name, value14.constructor.name]);
      };
    };
  };
  var readString = function(dictMonad) {
    return unsafeReadTagged(dictMonad)("String");
  };

  // output/Control.Promise/index.js
  var voidRight2 = /* @__PURE__ */ voidRight(functorEffect);
  var mempty2 = /* @__PURE__ */ mempty(monoidCanceler);
  var identity6 = /* @__PURE__ */ identity(categoryFn);
  var alt2 = /* @__PURE__ */ alt(/* @__PURE__ */ altExceptT(semigroupNonEmptyList)(monadIdentity));
  var unsafeReadTagged2 = /* @__PURE__ */ unsafeReadTagged(monadIdentity);
  var map9 = /* @__PURE__ */ map(/* @__PURE__ */ functorExceptT(functorIdentity));
  var readString2 = /* @__PURE__ */ readString(monadIdentity);
  var bind3 = /* @__PURE__ */ bind(bindAff);
  var liftEffect3 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var toAff$prime = function(customCoerce) {
    return function(p2) {
      return makeAff(function(cb) {
        return voidRight2(mempty2)(thenImpl(p2)(function($14) {
          return cb(Left.create(customCoerce($14)))();
        })(function($15) {
          return cb(Right.create($15))();
        }));
      });
    };
  };
  var coerce3 = function(fn) {
    return either(function(v) {
      return error("Promise failed, couldn't extract JS Error or String");
    })(identity6)(runExcept(alt2(unsafeReadTagged2("Error")(fn))(map9(error)(readString2(fn)))));
  };
  var toAff = /* @__PURE__ */ toAff$prime(coerce3);
  var toAffE = function(f) {
    return bind3(liftEffect3(f))(toAff);
  };

  // output/Graphics.Canvas.Extra/index.js
  var convertToJpegBlob = function(c) {
    return function(v) {
      return toAffE(_convertToJpegBlob(c)(v));
    };
  };

  // output/Web.DOM.Element/foreign.js
  var getProp = function(name15) {
    return function(doctype) {
      return doctype[name15];
    };
  };
  var _namespaceURI = getProp("namespaceURI");
  var _prefix = getProp("prefix");
  var localName = getProp("localName");
  var tagName = getProp("tagName");
  function getBoundingClientRect(el) {
    return function() {
      var rect2 = el.getBoundingClientRect();
      return {
        top: rect2.top,
        right: rect2.right,
        bottom: rect2.bottom,
        left: rect2.left,
        width: rect2.width,
        height: rect2.height,
        x: rect2.x,
        y: rect2.y
      };
    };
  }

  // output/Data.Nullable/foreign.js
  var nullImpl = null;
  function nullable(a2, r, f) {
    return a2 == null ? r : f(a2);
  }
  function notNull(x) {
    return x;
  }

  // output/Data.Nullable/index.js
  var toNullable = /* @__PURE__ */ maybe(nullImpl)(notNull);
  var toMaybe = function(n) {
    return nullable(n, Nothing.value, Just.create);
  };

  // output/Web.DOM.ParentNode/foreign.js
  var getEffProp = function(name15) {
    return function(node) {
      return function() {
        return node[name15];
      };
    };
  };
  var children = getEffProp("children");
  var _firstElementChild = getEffProp("firstElementChild");
  var _lastElementChild = getEffProp("lastElementChild");
  var childElementCount = getEffProp("childElementCount");
  function _querySelector(selector) {
    return function(node) {
      return function() {
        return node.querySelector(selector);
      };
    };
  }

  // output/Web.DOM.ParentNode/index.js
  var map10 = /* @__PURE__ */ map(functorEffect);
  var querySelector = function(qs) {
    var $2 = map10(toMaybe);
    var $3 = _querySelector(qs);
    return function($4) {
      return $2($3($4));
    };
  };

  // output/Web.DOM.Element/index.js
  var toNode = unsafeCoerce2;

  // output/Graphics.Canvas.Free/index.js
  var lift5 = /* @__PURE__ */ lift(monadTransReaderT);
  var map11 = /* @__PURE__ */ map(functorEffect);
  var discard2 = /* @__PURE__ */ discard(discardUnit);
  var map13 = /* @__PURE__ */ map(functorAff);
  var identity7 = /* @__PURE__ */ identity(categoryFn);
  var Font = function(x) {
    return x;
  };
  var Lift = /* @__PURE__ */ function() {
    function Lift4(value0) {
      this.value0 = value0;
    }
    ;
    Lift4.create = function(value0) {
      return new Lift4(value0);
    };
    return Lift4;
  }();
  var GetWidth = /* @__PURE__ */ function() {
    function GetWidth2(value0) {
      this.value0 = value0;
    }
    ;
    GetWidth2.create = function(value0) {
      return new GetWidth2(value0);
    };
    return GetWidth2;
  }();
  var SetWidth = /* @__PURE__ */ function() {
    function SetWidth2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SetWidth2.create = function(value0) {
      return function(value1) {
        return new SetWidth2(value0, value1);
      };
    };
    return SetWidth2;
  }();
  var GetHeight = /* @__PURE__ */ function() {
    function GetHeight2(value0) {
      this.value0 = value0;
    }
    ;
    GetHeight2.create = function(value0) {
      return new GetHeight2(value0);
    };
    return GetHeight2;
  }();
  var SetHeight = /* @__PURE__ */ function() {
    function SetHeight2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SetHeight2.create = function(value0) {
      return function(value1) {
        return new SetHeight2(value0, value1);
      };
    };
    return SetHeight2;
  }();
  var GetDimensions = /* @__PURE__ */ function() {
    function GetDimensions2(value0) {
      this.value0 = value0;
    }
    ;
    GetDimensions2.create = function(value0) {
      return new GetDimensions2(value0);
    };
    return GetDimensions2;
  }();
  var SetDimensions = /* @__PURE__ */ function() {
    function SetDimensions2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SetDimensions2.create = function(value0) {
      return function(value1) {
        return new SetDimensions2(value0, value1);
      };
    };
    return SetDimensions2;
  }();
  var GetBoundingClientRect = /* @__PURE__ */ function() {
    function GetBoundingClientRect2(value0) {
      this.value0 = value0;
    }
    ;
    GetBoundingClientRect2.create = function(value0) {
      return new GetBoundingClientRect2(value0);
    };
    return GetBoundingClientRect2;
  }();
  var ToDataURL = /* @__PURE__ */ function() {
    function ToDataURL2(value0) {
      this.value0 = value0;
    }
    ;
    ToDataURL2.create = function(value0) {
      return new ToDataURL2(value0);
    };
    return ToDataURL2;
  }();
  var SetLineWidth = /* @__PURE__ */ function() {
    function SetLineWidth2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SetLineWidth2.create = function(value0) {
      return function(value1) {
        return new SetLineWidth2(value0, value1);
      };
    };
    return SetLineWidth2;
  }();
  var SetLineDash = /* @__PURE__ */ function() {
    function SetLineDash2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SetLineDash2.create = function(value0) {
      return function(value1) {
        return new SetLineDash2(value0, value1);
      };
    };
    return SetLineDash2;
  }();
  var SetFillColor = /* @__PURE__ */ function() {
    function SetFillColor2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SetFillColor2.create = function(value0) {
      return function(value1) {
        return new SetFillColor2(value0, value1);
      };
    };
    return SetFillColor2;
  }();
  var SetStrokeColor = /* @__PURE__ */ function() {
    function SetStrokeColor2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SetStrokeColor2.create = function(value0) {
      return function(value1) {
        return new SetStrokeColor2(value0, value1);
      };
    };
    return SetStrokeColor2;
  }();
  var SetShadowBlurRadius = /* @__PURE__ */ function() {
    function SetShadowBlurRadius2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SetShadowBlurRadius2.create = function(value0) {
      return function(value1) {
        return new SetShadowBlurRadius2(value0, value1);
      };
    };
    return SetShadowBlurRadius2;
  }();
  var SetShadowOffsetX = /* @__PURE__ */ function() {
    function SetShadowOffsetX2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SetShadowOffsetX2.create = function(value0) {
      return function(value1) {
        return new SetShadowOffsetX2(value0, value1);
      };
    };
    return SetShadowOffsetX2;
  }();
  var SetShadowOffsetY = /* @__PURE__ */ function() {
    function SetShadowOffsetY2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SetShadowOffsetY2.create = function(value0) {
      return function(value1) {
        return new SetShadowOffsetY2(value0, value1);
      };
    };
    return SetShadowOffsetY2;
  }();
  var SetShadowColor = /* @__PURE__ */ function() {
    function SetShadowColor2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SetShadowColor2.create = function(value0) {
      return function(value1) {
        return new SetShadowColor2(value0, value1);
      };
    };
    return SetShadowColor2;
  }();
  var SetMiterLimit = /* @__PURE__ */ function() {
    function SetMiterLimit2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SetMiterLimit2.create = function(value0) {
      return function(value1) {
        return new SetMiterLimit2(value0, value1);
      };
    };
    return SetMiterLimit2;
  }();
  var SetLineCap = /* @__PURE__ */ function() {
    function SetLineCap2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SetLineCap2.create = function(value0) {
      return function(value1) {
        return new SetLineCap2(value0, value1);
      };
    };
    return SetLineCap2;
  }();
  var SetLineJoin = /* @__PURE__ */ function() {
    function SetLineJoin2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SetLineJoin2.create = function(value0) {
      return function(value1) {
        return new SetLineJoin2(value0, value1);
      };
    };
    return SetLineJoin2;
  }();
  var SetCompositeOperation = /* @__PURE__ */ function() {
    function SetCompositeOperation2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SetCompositeOperation2.create = function(value0) {
      return function(value1) {
        return new SetCompositeOperation2(value0, value1);
      };
    };
    return SetCompositeOperation2;
  }();
  var SetAlpha = /* @__PURE__ */ function() {
    function SetAlpha2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SetAlpha2.create = function(value0) {
      return function(value1) {
        return new SetAlpha2(value0, value1);
      };
    };
    return SetAlpha2;
  }();
  var BeginPath = /* @__PURE__ */ function() {
    function BeginPath2(value0) {
      this.value0 = value0;
    }
    ;
    BeginPath2.create = function(value0) {
      return new BeginPath2(value0);
    };
    return BeginPath2;
  }();
  var Stroke = /* @__PURE__ */ function() {
    function Stroke2(value0) {
      this.value0 = value0;
    }
    ;
    Stroke2.create = function(value0) {
      return new Stroke2(value0);
    };
    return Stroke2;
  }();
  var Fill = /* @__PURE__ */ function() {
    function Fill2(value0) {
      this.value0 = value0;
    }
    ;
    Fill2.create = function(value0) {
      return new Fill2(value0);
    };
    return Fill2;
  }();
  var Clip = /* @__PURE__ */ function() {
    function Clip2(value0) {
      this.value0 = value0;
    }
    ;
    Clip2.create = function(value0) {
      return new Clip2(value0);
    };
    return Clip2;
  }();
  var LineTo = /* @__PURE__ */ function() {
    function LineTo2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    LineTo2.create = function(value0) {
      return function(value1) {
        return new LineTo2(value0, value1);
      };
    };
    return LineTo2;
  }();
  var MoveTo = /* @__PURE__ */ function() {
    function MoveTo2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    MoveTo2.create = function(value0) {
      return function(value1) {
        return new MoveTo2(value0, value1);
      };
    };
    return MoveTo2;
  }();
  var ClosePath = /* @__PURE__ */ function() {
    function ClosePath2(value0) {
      this.value0 = value0;
    }
    ;
    ClosePath2.create = function(value0) {
      return new ClosePath2(value0);
    };
    return ClosePath2;
  }();
  var DrawArc = /* @__PURE__ */ function() {
    function DrawArc2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    DrawArc2.create = function(value0) {
      return function(value1) {
        return new DrawArc2(value0, value1);
      };
    };
    return DrawArc2;
  }();
  var Rect = /* @__PURE__ */ function() {
    function Rect2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Rect2.create = function(value0) {
      return function(value1) {
        return new Rect2(value0, value1);
      };
    };
    return Rect2;
  }();
  var FillRect = /* @__PURE__ */ function() {
    function FillRect2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    FillRect2.create = function(value0) {
      return function(value1) {
        return new FillRect2(value0, value1);
      };
    };
    return FillRect2;
  }();
  var StrokeRect = /* @__PURE__ */ function() {
    function StrokeRect2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    StrokeRect2.create = function(value0) {
      return function(value1) {
        return new StrokeRect2(value0, value1);
      };
    };
    return StrokeRect2;
  }();
  var ClearRect = /* @__PURE__ */ function() {
    function ClearRect2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ClearRect2.create = function(value0) {
      return function(value1) {
        return new ClearRect2(value0, value1);
      };
    };
    return ClearRect2;
  }();
  var Scale = /* @__PURE__ */ function() {
    function Scale2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Scale2.create = function(value0) {
      return function(value1) {
        return new Scale2(value0, value1);
      };
    };
    return Scale2;
  }();
  var Rotate = /* @__PURE__ */ function() {
    function Rotate2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Rotate2.create = function(value0) {
      return function(value1) {
        return new Rotate2(value0, value1);
      };
    };
    return Rotate2;
  }();
  var Translate = /* @__PURE__ */ function() {
    function Translate2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Translate2.create = function(value0) {
      return function(value1) {
        return new Translate2(value0, value1);
      };
    };
    return Translate2;
  }();
  var ApplyTransform = /* @__PURE__ */ function() {
    function ApplyTransform2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ApplyTransform2.create = function(value0) {
      return function(value1) {
        return new ApplyTransform2(value0, value1);
      };
    };
    return ApplyTransform2;
  }();
  var SetTransform = /* @__PURE__ */ function() {
    function SetTransform2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SetTransform2.create = function(value0) {
      return function(value1) {
        return new SetTransform2(value0, value1);
      };
    };
    return SetTransform2;
  }();
  var GetTextAlign = /* @__PURE__ */ function() {
    function GetTextAlign2(value0) {
      this.value0 = value0;
    }
    ;
    GetTextAlign2.create = function(value0) {
      return new GetTextAlign2(value0);
    };
    return GetTextAlign2;
  }();
  var SetTextAlign = /* @__PURE__ */ function() {
    function SetTextAlign2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SetTextAlign2.create = function(value0) {
      return function(value1) {
        return new SetTextAlign2(value0, value1);
      };
    };
    return SetTextAlign2;
  }();
  var GetTextBaseline = /* @__PURE__ */ function() {
    function GetTextBaseline2(value0) {
      this.value0 = value0;
    }
    ;
    GetTextBaseline2.create = function(value0) {
      return new GetTextBaseline2(value0);
    };
    return GetTextBaseline2;
  }();
  var SetTextBaseline = /* @__PURE__ */ function() {
    function SetTextBaseline2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SetTextBaseline2.create = function(value0) {
      return function(value1) {
        return new SetTextBaseline2(value0, value1);
      };
    };
    return SetTextBaseline2;
  }();
  var GetFont = /* @__PURE__ */ function() {
    function GetFont2(value0) {
      this.value0 = value0;
    }
    ;
    GetFont2.create = function(value0) {
      return new GetFont2(value0);
    };
    return GetFont2;
  }();
  var SetFont = /* @__PURE__ */ function() {
    function SetFont2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SetFont2.create = function(value0) {
      return function(value1) {
        return new SetFont2(value0, value1);
      };
    };
    return SetFont2;
  }();
  var FillText = /* @__PURE__ */ function() {
    function FillText2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    FillText2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new FillText2(value0, value1, value22);
        };
      };
    };
    return FillText2;
  }();
  var StrokeText = /* @__PURE__ */ function() {
    function StrokeText2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    StrokeText2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new StrokeText2(value0, value1, value22);
        };
      };
    };
    return StrokeText2;
  }();
  var MeasureText = /* @__PURE__ */ function() {
    function MeasureText2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    MeasureText2.create = function(value0) {
      return function(value1) {
        return new MeasureText2(value0, value1);
      };
    };
    return MeasureText2;
  }();
  var Save = /* @__PURE__ */ function() {
    function Save2(value0) {
      this.value0 = value0;
    }
    ;
    Save2.create = function(value0) {
      return new Save2(value0);
    };
    return Save2;
  }();
  var Restore = /* @__PURE__ */ function() {
    function Restore2(value0) {
      this.value0 = value0;
    }
    ;
    Restore2.create = function(value0) {
      return new Restore2(value0);
    };
    return Restore2;
  }();
  var GetImageData = /* @__PURE__ */ function() {
    function GetImageData2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    GetImageData2.create = function(value0) {
      return function(value1) {
        return new GetImageData2(value0, value1);
      };
    };
    return GetImageData2;
  }();
  var PutImageData = /* @__PURE__ */ function() {
    function PutImageData2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    PutImageData2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new PutImageData2(value0, value1, value22);
        };
      };
    };
    return PutImageData2;
  }();
  var PutImageDataRect = /* @__PURE__ */ function() {
    function PutImageDataRect2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    PutImageDataRect2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new PutImageDataRect2(value0, value1, value22, value32);
          };
        };
      };
    };
    return PutImageDataRect2;
  }();
  var BlankImageData = /* @__PURE__ */ function() {
    function BlankImageData2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    BlankImageData2.create = function(value0) {
      return function(value1) {
        return new BlankImageData2(value0, value1);
      };
    };
    return BlankImageData2;
  }();
  var ToImageSource = /* @__PURE__ */ function() {
    function ToImageSource2(value0) {
      this.value0 = value0;
    }
    ;
    ToImageSource2.create = function(value0) {
      return new ToImageSource2(value0);
    };
    return ToImageSource2;
  }();
  var DrawImage = /* @__PURE__ */ function() {
    function DrawImage2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    DrawImage2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new DrawImage2(value0, value1, value22);
        };
      };
    };
    return DrawImage2;
  }();
  var DrawImageScale = /* @__PURE__ */ function() {
    function DrawImageScale2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    DrawImageScale2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new DrawImageScale2(value0, value1, value22);
        };
      };
    };
    return DrawImageScale2;
  }();
  var DrawImageRectScale = /* @__PURE__ */ function() {
    function DrawImageRectScale2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    DrawImageRectScale2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new DrawImageRectScale2(value0, value1, value22);
        };
      };
    };
    return DrawImageRectScale2;
  }();
  var CreatePattern = /* @__PURE__ */ function() {
    function CreatePattern2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    CreatePattern2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new CreatePattern2(value0, value1, value22);
        };
      };
    };
    return CreatePattern2;
  }();
  var SetPatternFillStyle = /* @__PURE__ */ function() {
    function SetPatternFillStyle2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SetPatternFillStyle2.create = function(value0) {
      return function(value1) {
        return new SetPatternFillStyle2(value0, value1);
      };
    };
    return SetPatternFillStyle2;
  }();
  var CreateLinearGradient = /* @__PURE__ */ function() {
    function CreateLinearGradient2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CreateLinearGradient2.create = function(value0) {
      return function(value1) {
        return new CreateLinearGradient2(value0, value1);
      };
    };
    return CreateLinearGradient2;
  }();
  var CreateRadialGradient = /* @__PURE__ */ function() {
    function CreateRadialGradient2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CreateRadialGradient2.create = function(value0) {
      return function(value1) {
        return new CreateRadialGradient2(value0, value1);
      };
    };
    return CreateRadialGradient2;
  }();
  var SetGradientFillStyle = /* @__PURE__ */ function() {
    function SetGradientFillStyle2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SetGradientFillStyle2.create = function(value0) {
      return function(value1) {
        return new SetGradientFillStyle2(value0, value1);
      };
    };
    return SetGradientFillStyle2;
  }();
  var QuadraticCurveTo = /* @__PURE__ */ function() {
    function QuadraticCurveTo2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    QuadraticCurveTo2.create = function(value0) {
      return function(value1) {
        return new QuadraticCurveTo2(value0, value1);
      };
    };
    return QuadraticCurveTo2;
  }();
  var BezierCurveTo = /* @__PURE__ */ function() {
    function BezierCurveTo2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    BezierCurveTo2.create = function(value0) {
      return function(value1) {
        return new BezierCurveTo2(value0, value1);
      };
    };
    return BezierCurveTo2;
  }();
  var ConvertToJpegBlob = /* @__PURE__ */ function() {
    function ConvertToJpegBlob2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ConvertToJpegBlob2.create = function(value0) {
      return function(value1) {
        return new ConvertToJpegBlob2(value0, value1);
      };
    };
    return ConvertToJpegBlob2;
  }();
  var CanvasT = function(x) {
    return x;
  };
  var functorCanvasT = freeFunctor;
  var functorCanvasF = function(dictFunctor) {
    var map28 = map(dictFunctor);
    return {
      map: function(v) {
        return function(v1) {
          if (v1 instanceof Lift) {
            return new Lift(map28(v)(v1.value0));
          }
          ;
          if (v1 instanceof GetWidth) {
            return new GetWidth(function($517) {
              return v(v1.value0($517));
            });
          }
          ;
          if (v1 instanceof SetWidth) {
            return new SetWidth(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof GetHeight) {
            return new GetHeight(function($518) {
              return v(v1.value0($518));
            });
          }
          ;
          if (v1 instanceof SetHeight) {
            return new SetHeight(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof GetDimensions) {
            return new GetDimensions(function($519) {
              return v(v1.value0($519));
            });
          }
          ;
          if (v1 instanceof SetDimensions) {
            return new SetDimensions(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof GetBoundingClientRect) {
            return new GetBoundingClientRect(function($520) {
              return v(v1.value0($520));
            });
          }
          ;
          if (v1 instanceof ToDataURL) {
            return new ToDataURL(function($521) {
              return v(v1.value0($521));
            });
          }
          ;
          if (v1 instanceof SetLineWidth) {
            return new SetLineWidth(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof SetLineDash) {
            return new SetLineDash(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof SetFillColor) {
            return new SetFillColor(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof SetStrokeColor) {
            return new SetStrokeColor(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof SetShadowBlurRadius) {
            return new SetShadowBlurRadius(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof SetShadowOffsetX) {
            return new SetShadowOffsetX(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof SetShadowOffsetY) {
            return new SetShadowOffsetY(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof SetShadowColor) {
            return new SetShadowColor(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof SetMiterLimit) {
            return new SetMiterLimit(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof SetLineCap) {
            return new SetLineCap(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof SetLineJoin) {
            return new SetLineJoin(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof SetCompositeOperation) {
            return new SetCompositeOperation(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof SetAlpha) {
            return new SetAlpha(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof BeginPath) {
            return new BeginPath(v(v1.value0));
          }
          ;
          if (v1 instanceof Stroke) {
            return new Stroke(v(v1.value0));
          }
          ;
          if (v1 instanceof Fill) {
            return new Fill(v(v1.value0));
          }
          ;
          if (v1 instanceof Clip) {
            return new Clip(v(v1.value0));
          }
          ;
          if (v1 instanceof LineTo) {
            return new LineTo(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof MoveTo) {
            return new MoveTo(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof ClosePath) {
            return new ClosePath(v(v1.value0));
          }
          ;
          if (v1 instanceof DrawArc) {
            return new DrawArc(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof Rect) {
            return new Rect(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof FillRect) {
            return new FillRect(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof StrokeRect) {
            return new StrokeRect(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof ClearRect) {
            return new ClearRect(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof Scale) {
            return new Scale(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof Rotate) {
            return new Rotate(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof Translate) {
            return new Translate(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof ApplyTransform) {
            return new ApplyTransform(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof SetTransform) {
            return new SetTransform(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof GetTextAlign) {
            return new GetTextAlign(function($522) {
              return v(v1.value0($522));
            });
          }
          ;
          if (v1 instanceof SetTextAlign) {
            return new SetTextAlign(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof GetTextBaseline) {
            return new GetTextBaseline(function($523) {
              return v(v1.value0($523));
            });
          }
          ;
          if (v1 instanceof SetTextBaseline) {
            return new SetTextBaseline(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof GetFont) {
            return new GetFont(function($524) {
              return v(v1.value0($524));
            });
          }
          ;
          if (v1 instanceof SetFont) {
            return new SetFont(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof FillText) {
            return new FillText(v1.value0, v1.value1, v(v1.value2));
          }
          ;
          if (v1 instanceof StrokeText) {
            return new StrokeText(v1.value0, v1.value1, v(v1.value2));
          }
          ;
          if (v1 instanceof MeasureText) {
            return new MeasureText(v1.value0, function($525) {
              return v(v1.value1($525));
            });
          }
          ;
          if (v1 instanceof Save) {
            return new Save(v(v1.value0));
          }
          ;
          if (v1 instanceof Restore) {
            return new Restore(v(v1.value0));
          }
          ;
          if (v1 instanceof GetImageData) {
            return new GetImageData(v1.value0, function($526) {
              return v(v1.value1($526));
            });
          }
          ;
          if (v1 instanceof PutImageData) {
            return new PutImageData(v1.value0, v1.value1, v(v1.value2));
          }
          ;
          if (v1 instanceof PutImageDataRect) {
            return new PutImageDataRect(v1.value0, v1.value1, v1.value2, v(v1.value3));
          }
          ;
          if (v1 instanceof BlankImageData) {
            return new BlankImageData(v1.value0, function($527) {
              return v(v1.value1($527));
            });
          }
          ;
          if (v1 instanceof ToImageSource) {
            return new ToImageSource(function($528) {
              return v(v1.value0($528));
            });
          }
          ;
          if (v1 instanceof DrawImage) {
            return new DrawImage(v1.value0, v1.value1, v(v1.value2));
          }
          ;
          if (v1 instanceof DrawImageScale) {
            return new DrawImageScale(v1.value0, v1.value1, v(v1.value2));
          }
          ;
          if (v1 instanceof DrawImageRectScale) {
            return new DrawImageRectScale(v1.value0, v1.value1, v(v1.value2));
          }
          ;
          if (v1 instanceof CreatePattern) {
            return new CreatePattern(v1.value0, v1.value1, function($529) {
              return v(v1.value2($529));
            });
          }
          ;
          if (v1 instanceof SetPatternFillStyle) {
            return new SetPatternFillStyle(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof CreateLinearGradient) {
            return new CreateLinearGradient(v1.value0, function($530) {
              return v(v1.value1($530));
            });
          }
          ;
          if (v1 instanceof CreateRadialGradient) {
            return new CreateRadialGradient(v1.value0, function($531) {
              return v(v1.value1($531));
            });
          }
          ;
          if (v1 instanceof SetGradientFillStyle) {
            return new SetGradientFillStyle(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof QuadraticCurveTo) {
            return new QuadraticCurveTo(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof BezierCurveTo) {
            return new BezierCurveTo(v1.value0, v(v1.value1));
          }
          ;
          if (v1 instanceof ConvertToJpegBlob) {
            return new ConvertToJpegBlob(v1.value0, function($532) {
              return v(v1.value1($532));
            });
          }
          ;
          throw new Error("Failed pattern match at Graphics.Canvas.Free (line 582, column 1 - line 648, column 64): " + [v.constructor.name, v1.constructor.name]);
        };
      }
    };
  };
  var bindCanvasT = freeBind;
  var discard1 = /* @__PURE__ */ discard2(bindCanvasT);
  var bind4 = /* @__PURE__ */ bind(bindCanvasT);
  var applicativeCanvasT = freeApplicative;
  var pure4 = /* @__PURE__ */ pure(applicativeCanvasT);
  var runCanvasT = function(dictMonadAff) {
    var MonadEffect0 = dictMonadAff.MonadEffect0();
    var Monad0 = MonadEffect0.Monad0();
    var lift22 = lift5(Monad0);
    var Bind1 = Monad0.Bind1();
    var bindReaderT2 = bindReaderT(Bind1);
    var bind16 = bind(bindReaderT2);
    var ask2 = ask(monadAskReaderT(Monad0));
    var liftEffect8 = liftEffect(monadEffectReader(MonadEffect0));
    var discard24 = discard2(bindReaderT2);
    var pure13 = pure(applicativeReaderT(Monad0.Applicative0()));
    var liftAff2 = liftAff(monadAffReader(dictMonadAff));
    var runFreeM2 = runFreeM(functorCanvasF(Bind1.Apply0().Functor0()));
    return function(dictMonadRec) {
      var go2 = function(v) {
        if (v instanceof Lift) {
          return lift22(v.value0);
        }
        ;
        if (v instanceof GetWidth) {
          return bind16(ask2)(function(v1) {
            return liftEffect8(map11(v.value0)(getCanvasWidth(v1.canvasElement)));
          });
        }
        ;
        if (v instanceof SetWidth) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(setCanvasWidth(v1.canvasElement)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof GetHeight) {
          return bind16(ask2)(function(v1) {
            return liftEffect8(map11(v.value0)(getCanvasHeight(v1.canvasElement)));
          });
        }
        ;
        if (v instanceof SetHeight) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(setCanvasHeight(v1.canvasElement)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof GetDimensions) {
          return bind16(ask2)(function(v1) {
            return liftEffect8(map11(v.value0)(getCanvasDimensions(v1.canvasElement)));
          });
        }
        ;
        if (v instanceof SetDimensions) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(setCanvasDimensions(v1.canvasElement)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof GetBoundingClientRect) {
          return bind16(ask2)(function(v1) {
            return liftEffect8(map11(v.value0)(getBoundingClientRect(v1.canvasElement)));
          });
        }
        ;
        if (v instanceof ToDataURL) {
          return bind16(ask2)(function(v1) {
            return liftEffect8(map11(v.value0)(canvasToDataURL(v1.canvasElement)));
          });
        }
        ;
        if (v instanceof SetLineWidth) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(setLineWidth(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof SetLineDash) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(setLineDash(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof SetFillColor) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(setFillStyle(v1.context2D)(toHexString(v.value0))))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof SetStrokeColor) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(setStrokeStyle(v1.context2D)(toHexString(v.value0))))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof SetShadowBlurRadius) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(setShadowBlur(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof SetShadowOffsetX) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(setShadowOffsetX(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof SetShadowOffsetY) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(setShadowOffsetY(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof SetShadowColor) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(setShadowColor(v1.context2D)(toHexString(v.value0))))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof SetMiterLimit) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(setMiterLimit(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof SetLineCap) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(setLineCap(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof SetLineJoin) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(setLineJoin(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof SetCompositeOperation) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(setGlobalCompositeOperation(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof SetAlpha) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(setGlobalAlpha(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof BeginPath) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(beginPath(v1.context2D)))(function() {
              return pure13(v.value0);
            });
          });
        }
        ;
        if (v instanceof Stroke) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(stroke(v1.context2D)))(function() {
              return pure13(v.value0);
            });
          });
        }
        ;
        if (v instanceof Fill) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(fill(v1.context2D)))(function() {
              return pure13(v.value0);
            });
          });
        }
        ;
        if (v instanceof Clip) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(clip(v1.context2D)))(function() {
              return pure13(v.value0);
            });
          });
        }
        ;
        if (v instanceof LineTo) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(lineTo(v1.context2D)(v.value0.x)(v.value0.y)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof MoveTo) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(moveTo(v1.context2D)(v.value0.x)(v.value0.y)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof ClosePath) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(closePath(v1.context2D)))(function() {
              return pure13(v.value0);
            });
          });
        }
        ;
        if (v instanceof DrawArc) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(arc(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof Rect) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(rect(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof FillRect) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(fillRect(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof StrokeRect) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(strokeRect(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof ClearRect) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(clearRect(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof Scale) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(scale(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof Rotate) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(rotate(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof Translate) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(translate(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof ApplyTransform) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(transform(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof SetTransform) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(setTransform(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof GetTextAlign) {
          return bind16(ask2)(function(v1) {
            return liftEffect8(map11(v.value0)(textAlign(v1.context2D)));
          });
        }
        ;
        if (v instanceof SetTextAlign) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(setTextAlign(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof GetTextBaseline) {
          return bind16(ask2)(function(v1) {
            return liftEffect8(map11(v.value0)(textBaseline(v1.context2D)));
          });
        }
        ;
        if (v instanceof SetTextBaseline) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(setTextBaseline(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof GetFont) {
          return bind16(ask2)(function(v1) {
            return liftEffect8(map11(function($533) {
              return v.value0(Font($533));
            })(font(v1.context2D)));
          });
        }
        ;
        if (v instanceof SetFont) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(setFont(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof FillText) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(fillText(v1.context2D)(v.value0)(v.value1.x)(v.value1.y)))(function() {
              return pure13(v.value2);
            });
          });
        }
        ;
        if (v instanceof StrokeText) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(strokeText(v1.context2D)(v.value0)(v.value1.x)(v.value1.y)))(function() {
              return pure13(v.value2);
            });
          });
        }
        ;
        if (v instanceof MeasureText) {
          return bind16(ask2)(function(v1) {
            return liftEffect8(map11(v.value1)(measureText(v1.context2D)(v.value0)));
          });
        }
        ;
        if (v instanceof Save) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(save(v1.context2D)))(function() {
              return pure13(v.value0);
            });
          });
        }
        ;
        if (v instanceof Restore) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(restore(v1.context2D)))(function() {
              return pure13(v.value0);
            });
          });
        }
        ;
        if (v instanceof GetImageData) {
          return bind16(ask2)(function(v1) {
            return liftEffect8(map11(v.value1)(getImageData(v1.context2D)(v.value0.x)(v.value0.y)(v.value0.width)(v.value0.height)));
          });
        }
        ;
        if (v instanceof PutImageData) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(putImageData(v1.context2D)(v.value0)(v.value1.x)(v.value1.y)))(function() {
              return pure13(v.value2);
            });
          });
        }
        ;
        if (v instanceof PutImageDataRect) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(putImageDataFull(v1.context2D)(v.value0)(v.value1.x)(v.value1.y)(v.value2.x)(v.value2.y)(v.value2.width)(v.value2.height)))(function() {
              return pure13(v.value3);
            });
          });
        }
        ;
        if (v instanceof BlankImageData) {
          return bind16(ask2)(function(v1) {
            return liftEffect8(map11(v.value1)(createImageData(v1.context2D)(v.value0.width)(v.value0.height)));
          });
        }
        ;
        if (v instanceof ToImageSource) {
          return bind16(ask2)(function(v1) {
            return pure13(v.value0(canvasElementToImageSource(v1.canvasElement)));
          });
        }
        ;
        if (v instanceof DrawImage) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(drawImage(v1.context2D)(v.value0)(v.value1.x)(v.value1.y)))(function() {
              return pure13(v.value2);
            });
          });
        }
        ;
        if (v instanceof DrawImageScale) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(drawImageScale(v1.context2D)(v.value0)(v.value1.x)(v.value1.y)(v.value1.width)(v.value1.height)))(function() {
              return pure13(v.value2);
            });
          });
        }
        ;
        if (v instanceof DrawImageRectScale) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(drawImageFull(v1.context2D)(v.value0)(v.value1.source.x)(v.value1.source.y)(v.value1.source.width)(v.value1.source.height)(v.value1.target.x)(v.value1.target.y)(v.value1.target.width)(v.value1.target.height)))(function() {
              return pure13(v.value2);
            });
          });
        }
        ;
        if (v instanceof CreatePattern) {
          return bind16(ask2)(function(v1) {
            return liftEffect8(map11(v.value2)(createPattern(v1.context2D)(v.value0)(v.value1)));
          });
        }
        ;
        if (v instanceof SetPatternFillStyle) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(setPatternFillStyle(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof CreateLinearGradient) {
          return bind16(ask2)(function(v1) {
            return liftEffect8(map11(v.value1)(createLinearGradient(v1.context2D)(v.value0)));
          });
        }
        ;
        if (v instanceof CreateRadialGradient) {
          return bind16(ask2)(function(v1) {
            return liftEffect8(map11(v.value1)(createRadialGradient(v1.context2D)(v.value0)));
          });
        }
        ;
        if (v instanceof SetGradientFillStyle) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(setGradientFillStyle(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof QuadraticCurveTo) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(quadraticCurveTo(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof BezierCurveTo) {
          return bind16(ask2)(function(v1) {
            return discard24(liftEffect8(bezierCurveTo(v1.context2D)(v.value0)))(function() {
              return pure13(v.value1);
            });
          });
        }
        ;
        if (v instanceof ConvertToJpegBlob) {
          return bind16(ask2)(function(v1) {
            return liftAff2(map13(v.value1)(convertToJpegBlob(v1.canvasElement)(v.value0)));
          });
        }
        ;
        throw new Error("Failed pattern match at Graphics.Canvas.Free (line 61, column 5 - line 61, column 25): " + [v.constructor.name]);
      };
      var $534 = runFreeM2(monadRecReaderT(dictMonadRec))(go2);
      return function($535) {
        return $534(/* @__PURE__ */ function(v) {
          return v;
        }($535));
      };
    };
  };
  var liftC = function($536) {
    return CanvasT(liftF($536));
  };
  var lineTo2 = function(xy) {
    return liftC(new LineTo(xy, unit));
  };
  var moveTo2 = function(xy) {
    return liftC(new MoveTo(xy, unit));
  };
  var restore2 = /* @__PURE__ */ function() {
    return liftC(new Restore(unit));
  }();
  var save2 = /* @__PURE__ */ function() {
    return liftC(new Save(unit));
  }();
  var withContext = function(f) {
    return discard1(save2)(function() {
      return bind4(f)(function(a2) {
        return discard1(restore2)(function() {
          return pure4(a2);
        });
      });
    });
  };
  var setFillColor = function(c) {
    return liftC(new SetFillColor(c, unit));
  };
  var setLineWidth2 = function(w) {
    return liftC(new SetLineWidth(w, unit));
  };
  var setStrokeColor = function(c) {
    return liftC(new SetStrokeColor(c, unit));
  };
  var stroke2 = /* @__PURE__ */ function() {
    return liftC(new Stroke(unit));
  }();
  var getWidth = /* @__PURE__ */ function() {
    return liftC(new GetWidth(identity7));
  }();
  var getHeight = /* @__PURE__ */ function() {
    return liftC(new GetHeight(identity7));
  }();
  var getBoundingClientRect2 = /* @__PURE__ */ function() {
    return liftC(new GetBoundingClientRect(identity7));
  }();
  var fillRect2 = function(r) {
    return liftC(new FillRect(r, unit));
  };
  var clearRect2 = function(r) {
    return liftC(new ClearRect(r, unit));
  };
  var beginPath2 = /* @__PURE__ */ function() {
    return liftC(new BeginPath(unit));
  }();

  // output/Data.Coyoneda/index.js
  var CoyonedaF = /* @__PURE__ */ function() {
    function CoyonedaF2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CoyonedaF2.create = function(value0) {
      return function(value1) {
        return new CoyonedaF2(value0, value1);
      };
    };
    return CoyonedaF2;
  }();
  var unCoyoneda = function(f) {
    return function(v) {
      return runExists(function(v1) {
        return f(v1.value0)(v1.value1);
      })(v);
    };
  };
  var coyoneda = function(k) {
    return function(fi) {
      return mkExists(new CoyonedaF(k, fi));
    };
  };
  var functorCoyoneda = {
    map: function(f) {
      return function(v) {
        return runExists(function(v1) {
          return coyoneda(function($180) {
            return f(v1.value0($180));
          })(v1.value1);
        })(v);
      };
    }
  };
  var liftCoyoneda = /* @__PURE__ */ coyoneda(/* @__PURE__ */ identity(categoryFn));

  // output/Data.Map.Internal/index.js
  var $runtime_lazy4 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var map14 = /* @__PURE__ */ map(functorMaybe);
  var Leaf = /* @__PURE__ */ function() {
    function Leaf2() {
    }
    ;
    Leaf2.value = new Leaf2();
    return Leaf2;
  }();
  var Node = /* @__PURE__ */ function() {
    function Node2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    Node2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new Node2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return Node2;
  }();
  var Split = /* @__PURE__ */ function() {
    function Split2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Split2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Split2(value0, value1, value22);
        };
      };
    };
    return Split2;
  }();
  var SplitLast = /* @__PURE__ */ function() {
    function SplitLast2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    SplitLast2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new SplitLast2(value0, value1, value22);
        };
      };
    };
    return SplitLast2;
  }();
  var unsafeNode = function(k, v, l, r) {
    if (l instanceof Leaf) {
      if (r instanceof Leaf) {
        return new Node(1, 1, k, v, l, r);
      }
      ;
      if (r instanceof Node) {
        return new Node(1 + r.value0 | 0, 1 + r.value1 | 0, k, v, l, r);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 702, column 5 - line 706, column 39): " + [r.constructor.name]);
    }
    ;
    if (l instanceof Node) {
      if (r instanceof Leaf) {
        return new Node(1 + l.value0 | 0, 1 + l.value1 | 0, k, v, l, r);
      }
      ;
      if (r instanceof Node) {
        return new Node(1 + function() {
          var $280 = l.value0 > r.value0;
          if ($280) {
            return l.value0;
          }
          ;
          return r.value0;
        }() | 0, (1 + l.value1 | 0) + r.value1 | 0, k, v, l, r);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 708, column 5 - line 712, column 68): " + [r.constructor.name]);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 700, column 32 - line 712, column 68): " + [l.constructor.name]);
  };
  var singleton6 = function(k) {
    return function(v) {
      return new Node(1, 1, k, v, Leaf.value, Leaf.value);
    };
  };
  var unsafeBalancedNode = /* @__PURE__ */ function() {
    var height9 = function(v) {
      if (v instanceof Leaf) {
        return 0;
      }
      ;
      if (v instanceof Node) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 757, column 12 - line 759, column 26): " + [v.constructor.name]);
    };
    var rotateLeft = function(k, v, l, rk, rv, rl, rr) {
      if (rl instanceof Node && rl.value0 > height9(rr)) {
        return unsafeNode(rl.value2, rl.value3, unsafeNode(k, v, l, rl.value4), unsafeNode(rk, rv, rl.value5, rr));
      }
      ;
      return unsafeNode(rk, rv, unsafeNode(k, v, l, rl), rr);
    };
    var rotateRight = function(k, v, lk, lv, ll, lr, r) {
      if (lr instanceof Node && height9(ll) <= lr.value0) {
        return unsafeNode(lr.value2, lr.value3, unsafeNode(lk, lv, ll, lr.value4), unsafeNode(k, v, lr.value5, r));
      }
      ;
      return unsafeNode(lk, lv, ll, unsafeNode(k, v, lr, r));
    };
    return function(k, v, l, r) {
      if (l instanceof Leaf) {
        if (r instanceof Leaf) {
          return singleton6(k)(v);
        }
        ;
        if (r instanceof Node && r.value0 > 1) {
          return rotateLeft(k, v, l, r.value2, r.value3, r.value4, r.value5);
        }
        ;
        return unsafeNode(k, v, l, r);
      }
      ;
      if (l instanceof Node) {
        if (r instanceof Node) {
          if (r.value0 > (l.value0 + 1 | 0)) {
            return rotateLeft(k, v, l, r.value2, r.value3, r.value4, r.value5);
          }
          ;
          if (l.value0 > (r.value0 + 1 | 0)) {
            return rotateRight(k, v, l.value2, l.value3, l.value4, l.value5, r);
          }
          ;
        }
        ;
        if (r instanceof Leaf && l.value0 > 1) {
          return rotateRight(k, v, l.value2, l.value3, l.value4, l.value5, r);
        }
        ;
        return unsafeNode(k, v, l, r);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 717, column 40 - line 738, column 34): " + [l.constructor.name]);
    };
  }();
  var $lazy_unsafeSplit = /* @__PURE__ */ $runtime_lazy4("unsafeSplit", "Data.Map.Internal", function() {
    return function(comp, k, m) {
      if (m instanceof Leaf) {
        return new Split(Nothing.value, Leaf.value, Leaf.value);
      }
      ;
      if (m instanceof Node) {
        var v = comp(k)(m.value2);
        if (v instanceof LT) {
          var v1 = $lazy_unsafeSplit(793)(comp, k, m.value4);
          return new Split(v1.value0, v1.value1, unsafeBalancedNode(m.value2, m.value3, v1.value2, m.value5));
        }
        ;
        if (v instanceof GT) {
          var v1 = $lazy_unsafeSplit(796)(comp, k, m.value5);
          return new Split(v1.value0, unsafeBalancedNode(m.value2, m.value3, m.value4, v1.value1), v1.value2);
        }
        ;
        if (v instanceof EQ) {
          return new Split(new Just(m.value3), m.value4, m.value5);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 791, column 5 - line 799, column 30): " + [v.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 787, column 34 - line 799, column 30): " + [m.constructor.name]);
    };
  });
  var unsafeSplit = /* @__PURE__ */ $lazy_unsafeSplit(786);
  var $lazy_unsafeSplitLast = /* @__PURE__ */ $runtime_lazy4("unsafeSplitLast", "Data.Map.Internal", function() {
    return function(k, v, l, r) {
      if (r instanceof Leaf) {
        return new SplitLast(k, v, l);
      }
      ;
      if (r instanceof Node) {
        var v1 = $lazy_unsafeSplitLast(779)(r.value2, r.value3, r.value4, r.value5);
        return new SplitLast(v1.value0, v1.value1, unsafeBalancedNode(k, v, l, v1.value2));
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 776, column 37 - line 780, column 57): " + [r.constructor.name]);
    };
  });
  var unsafeSplitLast = /* @__PURE__ */ $lazy_unsafeSplitLast(775);
  var unsafeJoinNodes = function(v, v1) {
    if (v instanceof Leaf) {
      return v1;
    }
    ;
    if (v instanceof Node) {
      var v2 = unsafeSplitLast(v.value2, v.value3, v.value4, v.value5);
      return unsafeBalancedNode(v2.value0, v2.value1, v2.value2, v1);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 764, column 25 - line 768, column 38): " + [v.constructor.name, v1.constructor.name]);
  };
  var pop = function(dictOrd) {
    var compare2 = compare(dictOrd);
    return function(k) {
      return function(m) {
        var v = unsafeSplit(compare2, k, m);
        return map14(function(a2) {
          return new Tuple(a2, unsafeJoinNodes(v.value1, v.value2));
        })(v.value0);
      };
    };
  };
  var lookup3 = function(dictOrd) {
    var compare2 = compare(dictOrd);
    return function(k) {
      var go2 = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof Leaf) {
            $tco_done = true;
            return Nothing.value;
          }
          ;
          if (v instanceof Node) {
            var v1 = compare2(k)(v.value2);
            if (v1 instanceof LT) {
              $copy_v = v.value4;
              return;
            }
            ;
            if (v1 instanceof GT) {
              $copy_v = v.value5;
              return;
            }
            ;
            if (v1 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value3);
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 283, column 7 - line 286, column 22): " + [v1.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 280, column 8 - line 286, column 22): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return go2;
    };
  };
  var insert2 = function(dictOrd) {
    var compare2 = compare(dictOrd);
    return function(k) {
      return function(v) {
        var go2 = function(v1) {
          if (v1 instanceof Leaf) {
            return singleton6(k)(v);
          }
          ;
          if (v1 instanceof Node) {
            var v2 = compare2(k)(v1.value2);
            if (v2 instanceof LT) {
              return unsafeBalancedNode(v1.value2, v1.value3, go2(v1.value4), v1.value5);
            }
            ;
            if (v2 instanceof GT) {
              return unsafeBalancedNode(v1.value2, v1.value3, v1.value4, go2(v1.value5));
            }
            ;
            if (v2 instanceof EQ) {
              return new Node(v1.value0, v1.value1, k, v, v1.value4, v1.value5);
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 471, column 7 - line 474, column 35): " + [v2.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 468, column 8 - line 474, column 35): " + [v1.constructor.name]);
        };
        return go2;
      };
    };
  };
  var foldableMap = {
    foldr: function(f) {
      return function(z) {
        var $lazy_go = $runtime_lazy4("go", "Data.Map.Internal", function() {
          return function(m$prime, z$prime) {
            if (m$prime instanceof Leaf) {
              return z$prime;
            }
            ;
            if (m$prime instanceof Node) {
              return $lazy_go(172)(m$prime.value4, f(m$prime.value3)($lazy_go(172)(m$prime.value5, z$prime)));
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 169, column 26 - line 172, column 43): " + [m$prime.constructor.name]);
          };
        });
        var go2 = $lazy_go(169);
        return function(m) {
          return go2(m, z);
        };
      };
    },
    foldl: function(f) {
      return function(z) {
        var $lazy_go = $runtime_lazy4("go", "Data.Map.Internal", function() {
          return function(z$prime, m$prime) {
            if (m$prime instanceof Leaf) {
              return z$prime;
            }
            ;
            if (m$prime instanceof Node) {
              return $lazy_go(178)(f($lazy_go(178)(z$prime, m$prime.value4))(m$prime.value3), m$prime.value5);
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 175, column 26 - line 178, column 43): " + [m$prime.constructor.name]);
          };
        });
        var go2 = $lazy_go(175);
        return function(m) {
          return go2(z, m);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty3 = mempty(dictMonoid);
      var append13 = append(dictMonoid.Semigroup0());
      return function(f) {
        var go2 = function(v) {
          if (v instanceof Leaf) {
            return mempty3;
          }
          ;
          if (v instanceof Node) {
            return append13(go2(v.value4))(append13(f(v.value3))(go2(v.value5)));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 181, column 10 - line 184, column 28): " + [v.constructor.name]);
        };
        return go2;
      };
    }
  };
  var empty4 = /* @__PURE__ */ function() {
    return Leaf.value;
  }();
  var $$delete2 = function(dictOrd) {
    var compare2 = compare(dictOrd);
    return function(k) {
      var go2 = function(v) {
        if (v instanceof Leaf) {
          return Leaf.value;
        }
        ;
        if (v instanceof Node) {
          var v1 = compare2(k)(v.value2);
          if (v1 instanceof LT) {
            return unsafeBalancedNode(v.value2, v.value3, go2(v.value4), v.value5);
          }
          ;
          if (v1 instanceof GT) {
            return unsafeBalancedNode(v.value2, v.value3, v.value4, go2(v.value5));
          }
          ;
          if (v1 instanceof EQ) {
            return unsafeJoinNodes(v.value4, v.value5);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 7 - line 501, column 43): " + [v1.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 8 - line 501, column 43): " + [v.constructor.name]);
      };
      return go2;
    };
  };
  var alter = function(dictOrd) {
    var compare2 = compare(dictOrd);
    return function(f) {
      return function(k) {
        return function(m) {
          var v = unsafeSplit(compare2, k, m);
          var v2 = f(v.value0);
          if (v2 instanceof Nothing) {
            return unsafeJoinNodes(v.value1, v.value2);
          }
          ;
          if (v2 instanceof Just) {
            return unsafeBalancedNode(k, v2.value0, v.value1, v.value2);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 514, column 3 - line 518, column 41): " + [v2.constructor.name]);
        };
      };
    };
  };

  // output/Halogen.Data.OrdBox/index.js
  var OrdBox = /* @__PURE__ */ function() {
    function OrdBox2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    OrdBox2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new OrdBox2(value0, value1, value22);
        };
      };
    };
    return OrdBox2;
  }();
  var mkOrdBox = function(dictOrd) {
    return OrdBox.create(eq(dictOrd.Eq0()))(compare(dictOrd));
  };
  var eqOrdBox = {
    eq: function(v) {
      return function(v1) {
        return v.value0(v.value2)(v1.value2);
      };
    }
  };
  var ordOrdBox = {
    compare: function(v) {
      return function(v1) {
        return v.value1(v.value2)(v1.value2);
      };
    },
    Eq0: function() {
      return eqOrdBox;
    }
  };

  // output/Halogen.Data.Slot/index.js
  var ordTuple2 = /* @__PURE__ */ ordTuple(ordString)(ordOrdBox);
  var pop1 = /* @__PURE__ */ pop(ordTuple2);
  var lookup1 = /* @__PURE__ */ lookup3(ordTuple2);
  var insert1 = /* @__PURE__ */ insert2(ordTuple2);
  var pop2 = function() {
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return function(dictOrd) {
        var mkOrdBox2 = mkOrdBox(dictOrd);
        return function(sym) {
          return function(key5) {
            return function(v) {
              return pop1(new Tuple(reflectSymbol2(sym), mkOrdBox2(key5)))(v);
            };
          };
        };
      };
    };
  };
  var lookup4 = function() {
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return function(dictOrd) {
        var mkOrdBox2 = mkOrdBox(dictOrd);
        return function(sym) {
          return function(key5) {
            return function(v) {
              return lookup1(new Tuple(reflectSymbol2(sym), mkOrdBox2(key5)))(v);
            };
          };
        };
      };
    };
  };
  var insert3 = function() {
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return function(dictOrd) {
        var mkOrdBox2 = mkOrdBox(dictOrd);
        return function(sym) {
          return function(key5) {
            return function(val) {
              return function(v) {
                return insert1(new Tuple(reflectSymbol2(sym), mkOrdBox2(key5)))(val)(v);
              };
            };
          };
        };
      };
    };
  };
  var foreachSlot = function(dictApplicative) {
    var traverse_9 = traverse_(dictApplicative)(foldableMap);
    return function(v) {
      return function(k) {
        return traverse_9(function($54) {
          return k($54);
        })(v);
      };
    };
  };
  var empty5 = empty4;

  // output/Halogen.Query.Input/index.js
  var RefUpdate = /* @__PURE__ */ function() {
    function RefUpdate2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    RefUpdate2.create = function(value0) {
      return function(value1) {
        return new RefUpdate2(value0, value1);
      };
    };
    return RefUpdate2;
  }();
  var Action = /* @__PURE__ */ function() {
    function Action3(value0) {
      this.value0 = value0;
    }
    ;
    Action3.create = function(value0) {
      return new Action3(value0);
    };
    return Action3;
  }();

  // output/Halogen.VDom.Machine/index.js
  var Step = /* @__PURE__ */ function() {
    function Step3(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Step3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Step3(value0, value1, value22, value32);
          };
        };
      };
    };
    return Step3;
  }();
  var unStep = unsafeCoerce2;
  var step = function(v, a2) {
    return v.value2(v.value1, a2);
  };
  var mkStep = unsafeCoerce2;
  var halt = function(v) {
    return v.value3(v.value1);
  };
  var extract2 = /* @__PURE__ */ unStep(function(v) {
    return v.value0;
  });

  // output/Halogen.VDom.Types/index.js
  var map15 = /* @__PURE__ */ map(functorArray);
  var map16 = /* @__PURE__ */ map(functorTuple);
  var Text = /* @__PURE__ */ function() {
    function Text2(value0) {
      this.value0 = value0;
    }
    ;
    Text2.create = function(value0) {
      return new Text2(value0);
    };
    return Text2;
  }();
  var Elem = /* @__PURE__ */ function() {
    function Elem3(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Elem3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Elem3(value0, value1, value22, value32);
          };
        };
      };
    };
    return Elem3;
  }();
  var Keyed = /* @__PURE__ */ function() {
    function Keyed2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Keyed2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Keyed2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Keyed2;
  }();
  var Widget = /* @__PURE__ */ function() {
    function Widget2(value0) {
      this.value0 = value0;
    }
    ;
    Widget2.create = function(value0) {
      return new Widget2(value0);
    };
    return Widget2;
  }();
  var Grafted = /* @__PURE__ */ function() {
    function Grafted2(value0) {
      this.value0 = value0;
    }
    ;
    Grafted2.create = function(value0) {
      return new Grafted2(value0);
    };
    return Grafted2;
  }();
  var Graft = /* @__PURE__ */ function() {
    function Graft2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Graft2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Graft2(value0, value1, value22);
        };
      };
    };
    return Graft2;
  }();
  var unGraft = function(f) {
    return function($61) {
      return f($61);
    };
  };
  var graft = unsafeCoerce2;
  var bifunctorGraft = {
    bimap: function(f) {
      return function(g) {
        return unGraft(function(v) {
          return graft(new Graft(function($63) {
            return f(v.value0($63));
          }, function($64) {
            return g(v.value1($64));
          }, v.value2));
        });
      };
    }
  };
  var bimap2 = /* @__PURE__ */ bimap(bifunctorGraft);
  var runGraft = /* @__PURE__ */ unGraft(function(v) {
    var go2 = function(v2) {
      if (v2 instanceof Text) {
        return new Text(v2.value0);
      }
      ;
      if (v2 instanceof Elem) {
        return new Elem(v2.value0, v2.value1, v.value0(v2.value2), map15(go2)(v2.value3));
      }
      ;
      if (v2 instanceof Keyed) {
        return new Keyed(v2.value0, v2.value1, v.value0(v2.value2), map15(map16(go2))(v2.value3));
      }
      ;
      if (v2 instanceof Widget) {
        return new Widget(v.value1(v2.value0));
      }
      ;
      if (v2 instanceof Grafted) {
        return new Grafted(bimap2(v.value0)(v.value1)(v2.value0));
      }
      ;
      throw new Error("Failed pattern match at Halogen.VDom.Types (line 86, column 7 - line 86, column 27): " + [v2.constructor.name]);
    };
    return go2(v.value2);
  });

  // output/Halogen.VDom.Util/foreign.js
  function unsafeGetAny(key5, obj) {
    return obj[key5];
  }
  function unsafeHasAny(key5, obj) {
    return obj.hasOwnProperty(key5);
  }
  function unsafeSetAny(key5, val, obj) {
    obj[key5] = val;
  }
  function forE2(a2, f) {
    var b2 = [];
    for (var i2 = 0; i2 < a2.length; i2++) {
      b2.push(f(i2, a2[i2]));
    }
    return b2;
  }
  function forEachE(a2, f) {
    for (var i2 = 0; i2 < a2.length; i2++) {
      f(a2[i2]);
    }
  }
  function forInE(o, f) {
    var ks = Object.keys(o);
    for (var i2 = 0; i2 < ks.length; i2++) {
      var k = ks[i2];
      f(k, o[k]);
    }
  }
  function diffWithIxE(a1, a2, f1, f2, f3) {
    var a3 = [];
    var l1 = a1.length;
    var l2 = a2.length;
    var i2 = 0;
    while (1) {
      if (i2 < l1) {
        if (i2 < l2) {
          a3.push(f1(i2, a1[i2], a2[i2]));
        } else {
          f2(i2, a1[i2]);
        }
      } else if (i2 < l2) {
        a3.push(f3(i2, a2[i2]));
      } else {
        break;
      }
      i2++;
    }
    return a3;
  }
  function strMapWithIxE(as, fk, f) {
    var o = {};
    for (var i2 = 0; i2 < as.length; i2++) {
      var a2 = as[i2];
      var k = fk(a2);
      o[k] = f(k, i2, a2);
    }
    return o;
  }
  function diffWithKeyAndIxE(o1, as, fk, f1, f2, f3) {
    var o2 = {};
    for (var i2 = 0; i2 < as.length; i2++) {
      var a2 = as[i2];
      var k = fk(a2);
      if (o1.hasOwnProperty(k)) {
        o2[k] = f1(k, i2, o1[k], a2);
      } else {
        o2[k] = f3(k, i2, a2);
      }
    }
    for (var k in o1) {
      if (k in o2) {
        continue;
      }
      f2(k, o1[k]);
    }
    return o2;
  }
  function refEq2(a2, b2) {
    return a2 === b2;
  }
  function createTextNode(s, doc) {
    return doc.createTextNode(s);
  }
  function setTextContent(s, n) {
    n.textContent = s;
  }
  function createElement(ns, name15, doc) {
    if (ns != null) {
      return doc.createElementNS(ns, name15);
    } else {
      return doc.createElement(name15);
    }
  }
  function insertChildIx(i2, a2, b2) {
    var n = b2.childNodes.item(i2) || null;
    if (n !== a2) {
      b2.insertBefore(a2, n);
    }
  }
  function removeChild(a2, b2) {
    if (b2 && a2.parentNode === b2) {
      b2.removeChild(a2);
    }
  }
  function parentNode(a2) {
    return a2.parentNode;
  }
  function setAttribute2(ns, attr3, val, el) {
    if (ns != null) {
      el.setAttributeNS(ns, attr3, val);
    } else {
      el.setAttribute(attr3, val);
    }
  }
  function removeAttribute2(ns, attr3, el) {
    if (ns != null) {
      el.removeAttributeNS(ns, attr3);
    } else {
      el.removeAttribute(attr3);
    }
  }
  function hasAttribute2(ns, attr3, el) {
    if (ns != null) {
      return el.hasAttributeNS(ns, attr3);
    } else {
      return el.hasAttribute(attr3);
    }
  }
  function addEventListener(ev, listener, el) {
    el.addEventListener(ev, listener, false);
  }
  function removeEventListener(ev, listener, el) {
    el.removeEventListener(ev, listener, false);
  }
  var jsUndefined = void 0;

  // output/Foreign.Object.ST/foreign.js
  var newImpl = function() {
    return {};
  };
  function poke2(k) {
    return function(v) {
      return function(m) {
        return function() {
          m[k] = v;
          return m;
        };
      };
    };
  }

  // output/Halogen.VDom.Util/index.js
  var unsafeLookup = unsafeGetAny;
  var unsafeFreeze2 = unsafeCoerce2;
  var pokeMutMap = unsafeSetAny;
  var newMutMap = newImpl;

  // output/Halogen.VDom.DOM/index.js
  var $runtime_lazy5 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var haltWidget = function(v) {
    return halt(v.widget);
  };
  var $lazy_patchWidget = /* @__PURE__ */ $runtime_lazy5("patchWidget", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchWidget(291)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Widget) {
        var res = step(state3.widget, vdom.value0);
        var res$prime = unStep(function(v) {
          return mkStep(new Step(v.value0, {
            build: state3.build,
            widget: res
          }, $lazy_patchWidget(296), haltWidget));
        })(res);
        return res$prime;
      }
      ;
      haltWidget(state3);
      return state3.build(vdom);
    };
  });
  var patchWidget = /* @__PURE__ */ $lazy_patchWidget(286);
  var haltText = function(v) {
    var parent2 = parentNode(v.node);
    return removeChild(v.node, parent2);
  };
  var $lazy_patchText = /* @__PURE__ */ $runtime_lazy5("patchText", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchText(82)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Text) {
        if (state3.value === vdom.value0) {
          return mkStep(new Step(state3.node, state3, $lazy_patchText(85), haltText));
        }
        ;
        if (otherwise) {
          var nextState = {
            build: state3.build,
            node: state3.node,
            value: vdom.value0
          };
          setTextContent(vdom.value0, state3.node);
          return mkStep(new Step(state3.node, nextState, $lazy_patchText(89), haltText));
        }
        ;
      }
      ;
      haltText(state3);
      return state3.build(vdom);
    };
  });
  var patchText = /* @__PURE__ */ $lazy_patchText(77);
  var haltKeyed = function(v) {
    var parent2 = parentNode(v.node);
    removeChild(v.node, parent2);
    forInE(v.children, function(v1, s) {
      return halt(s);
    });
    return halt(v.attrs);
  };
  var haltElem = function(v) {
    var parent2 = parentNode(v.node);
    removeChild(v.node, parent2);
    forEachE(v.children, halt);
    return halt(v.attrs);
  };
  var eqElemSpec = function(ns1, v, ns2, v1) {
    var $63 = v === v1;
    if ($63) {
      if (ns1 instanceof Just && (ns2 instanceof Just && ns1.value0 === ns2.value0)) {
        return true;
      }
      ;
      if (ns1 instanceof Nothing && ns2 instanceof Nothing) {
        return true;
      }
      ;
      return false;
    }
    ;
    return false;
  };
  var $lazy_patchElem = /* @__PURE__ */ $runtime_lazy5("patchElem", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchElem(135)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Elem && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
        var v = length(vdom.value3);
        var v1 = length(state3.children);
        if (v1 === 0 && v === 0) {
          var attrs2 = step(state3.attrs, vdom.value2);
          var nextState = {
            build: state3.build,
            node: state3.node,
            attrs: attrs2,
            ns: vdom.value0,
            name: vdom.value1,
            children: state3.children
          };
          return mkStep(new Step(state3.node, nextState, $lazy_patchElem(149), haltElem));
        }
        ;
        var onThis = function(v2, s) {
          return halt(s);
        };
        var onThese = function(ix, s, v2) {
          var res = step(s, v2);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var onThat = function(ix, v2) {
          var res = state3.build(v2);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var children2 = diffWithIxE(state3.children, vdom.value3, onThese, onThis, onThat);
        var attrs2 = step(state3.attrs, vdom.value2);
        var nextState = {
          build: state3.build,
          node: state3.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: children2
        };
        return mkStep(new Step(state3.node, nextState, $lazy_patchElem(172), haltElem));
      }
      ;
      haltElem(state3);
      return state3.build(vdom);
    };
  });
  var patchElem = /* @__PURE__ */ $lazy_patchElem(130);
  var $lazy_patchKeyed = /* @__PURE__ */ $runtime_lazy5("patchKeyed", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchKeyed(222)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Keyed && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
        var v = length(vdom.value3);
        if (state3.length === 0 && v === 0) {
          var attrs2 = step(state3.attrs, vdom.value2);
          var nextState = {
            build: state3.build,
            node: state3.node,
            attrs: attrs2,
            ns: vdom.value0,
            name: vdom.value1,
            children: state3.children,
            length: 0
          };
          return mkStep(new Step(state3.node, nextState, $lazy_patchKeyed(237), haltKeyed));
        }
        ;
        var onThis = function(v2, s) {
          return halt(s);
        };
        var onThese = function(v2, ix$prime, s, v3) {
          var res = step(s, v3.value1);
          insertChildIx(ix$prime, extract2(res), state3.node);
          return res;
        };
        var onThat = function(v2, ix, v3) {
          var res = state3.build(v3.value1);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var children2 = diffWithKeyAndIxE(state3.children, vdom.value3, fst, onThese, onThis, onThat);
        var attrs2 = step(state3.attrs, vdom.value2);
        var nextState = {
          build: state3.build,
          node: state3.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: children2,
          length: v
        };
        return mkStep(new Step(state3.node, nextState, $lazy_patchKeyed(261), haltKeyed));
      }
      ;
      haltKeyed(state3);
      return state3.build(vdom);
    };
  });
  var patchKeyed = /* @__PURE__ */ $lazy_patchKeyed(217);
  var buildWidget = function(v, build, w) {
    var res = v.buildWidget(v)(w);
    var res$prime = unStep(function(v1) {
      return mkStep(new Step(v1.value0, {
        build,
        widget: res
      }, patchWidget, haltWidget));
    })(res);
    return res$prime;
  };
  var buildText = function(v, build, s) {
    var node = createTextNode(s, v.document);
    var state3 = {
      build,
      node,
      value: s
    };
    return mkStep(new Step(node, state3, patchText, haltText));
  };
  var buildKeyed = function(v, build, ns1, name1, as1, ch1) {
    var el = createElement(toNullable(ns1), name1, v.document);
    var node = toNode(el);
    var onChild = function(v1, ix, v2) {
      var res = build(v2.value1);
      insertChildIx(ix, extract2(res), node);
      return res;
    };
    var children2 = strMapWithIxE(ch1, fst, onChild);
    var attrs = v.buildAttributes(el)(as1);
    var state3 = {
      build,
      node,
      attrs,
      ns: ns1,
      name: name1,
      children: children2,
      length: length(ch1)
    };
    return mkStep(new Step(node, state3, patchKeyed, haltKeyed));
  };
  var buildElem = function(v, build, ns1, name1, as1, ch1) {
    var el = createElement(toNullable(ns1), name1, v.document);
    var node = toNode(el);
    var onChild = function(ix, child2) {
      var res = build(child2);
      insertChildIx(ix, extract2(res), node);
      return res;
    };
    var children2 = forE2(ch1, onChild);
    var attrs = v.buildAttributes(el)(as1);
    var state3 = {
      build,
      node,
      attrs,
      ns: ns1,
      name: name1,
      children: children2
    };
    return mkStep(new Step(node, state3, patchElem, haltElem));
  };
  var buildVDom = function(spec) {
    var $lazy_build = $runtime_lazy5("build", "Halogen.VDom.DOM", function() {
      return function(v) {
        if (v instanceof Text) {
          return buildText(spec, $lazy_build(59), v.value0);
        }
        ;
        if (v instanceof Elem) {
          return buildElem(spec, $lazy_build(60), v.value0, v.value1, v.value2, v.value3);
        }
        ;
        if (v instanceof Keyed) {
          return buildKeyed(spec, $lazy_build(61), v.value0, v.value1, v.value2, v.value3);
        }
        ;
        if (v instanceof Widget) {
          return buildWidget(spec, $lazy_build(62), v.value0);
        }
        ;
        if (v instanceof Grafted) {
          return $lazy_build(63)(runGraft(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Halogen.VDom.DOM (line 58, column 27 - line 63, column 52): " + [v.constructor.name]);
      };
    });
    var build = $lazy_build(58);
    return build;
  };

  // output/Foreign.Object/foreign.js
  function runST(f) {
    return f();
  }
  function _foldM(bind16) {
    return function(f) {
      return function(mz) {
        return function(m) {
          var acc = mz;
          function g(k2) {
            return function(z) {
              return f(z)(k2)(m[k2]);
            };
          }
          for (var k in m) {
            if (hasOwnProperty.call(m, k)) {
              acc = bind16(acc)(g(k));
            }
          }
          return acc;
        };
      };
    };
  }
  function _lookup(no, yes, k, m) {
    return k in m ? yes(m[k]) : no;
  }
  function toArrayWithKey(f) {
    return function(m) {
      var r = [];
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r.push(f(k)(m[k]));
        }
      }
      return r;
    };
  }
  var keys = Object.keys || toArrayWithKey(function(k) {
    return function() {
      return k;
    };
  });

  // output/Foreign.Object/index.js
  var $$void4 = /* @__PURE__ */ $$void(functorST);
  var lookup5 = /* @__PURE__ */ function() {
    return runFn4(_lookup)(Nothing.value)(Just.create);
  }();
  var fromFoldable3 = function(dictFoldable) {
    var fromFoldable1 = fromFoldable(dictFoldable);
    return function(l) {
      return runST(function __do2() {
        var s = newImpl();
        foreach(fromFoldable1(l))(function(v) {
          return $$void4(poke2(v.value0)(v.value1)(s));
        })();
        return s;
      });
    };
  };
  var fold2 = /* @__PURE__ */ _foldM(applyFlipped);
  var foldMap2 = function(dictMonoid) {
    var append13 = append(dictMonoid.Semigroup0());
    var mempty3 = mempty(dictMonoid);
    return function(f) {
      return fold2(function(acc) {
        return function(k) {
          return function(v) {
            return append13(acc)(f(k)(v));
          };
        };
      })(mempty3);
    };
  };

  // output/Web.Event.EventTarget/foreign.js
  function eventListener(fn) {
    return function() {
      return function(event) {
        return fn(event)();
      };
    };
  }
  function addEventListener2(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target7) {
          return function() {
            return target7.addEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }
  function removeEventListener2(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target7) {
          return function() {
            return target7.removeEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }

  // output/Halogen.VDom.DOM.Prop/index.js
  var $runtime_lazy6 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var Created = /* @__PURE__ */ function() {
    function Created2(value0) {
      this.value0 = value0;
    }
    ;
    Created2.create = function(value0) {
      return new Created2(value0);
    };
    return Created2;
  }();
  var Removed = /* @__PURE__ */ function() {
    function Removed2(value0) {
      this.value0 = value0;
    }
    ;
    Removed2.create = function(value0) {
      return new Removed2(value0);
    };
    return Removed2;
  }();
  var Attribute = /* @__PURE__ */ function() {
    function Attribute2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Attribute2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Attribute2(value0, value1, value22);
        };
      };
    };
    return Attribute2;
  }();
  var Property2 = /* @__PURE__ */ function() {
    function Property3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Property3.create = function(value0) {
      return function(value1) {
        return new Property3(value0, value1);
      };
    };
    return Property3;
  }();
  var Handler = /* @__PURE__ */ function() {
    function Handler2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Handler2.create = function(value0) {
      return function(value1) {
        return new Handler2(value0, value1);
      };
    };
    return Handler2;
  }();
  var Ref = /* @__PURE__ */ function() {
    function Ref2(value0) {
      this.value0 = value0;
    }
    ;
    Ref2.create = function(value0) {
      return new Ref2(value0);
    };
    return Ref2;
  }();
  var unsafeGetProperty = unsafeGetAny;
  var setProperty = unsafeSetAny;
  var removeProperty = function(key5, el) {
    var v = hasAttribute2(nullImpl, key5, el);
    if (v) {
      return removeAttribute2(nullImpl, key5, el);
    }
    ;
    var v1 = typeOf(unsafeGetAny(key5, el));
    if (v1 === "string") {
      return unsafeSetAny(key5, "", el);
    }
    ;
    if (key5 === "rowSpan") {
      return unsafeSetAny(key5, 1, el);
    }
    ;
    if (key5 === "colSpan") {
      return unsafeSetAny(key5, 1, el);
    }
    ;
    return unsafeSetAny(key5, jsUndefined, el);
  };
  var propToStrKey = function(v) {
    if (v instanceof Attribute && v.value0 instanceof Just) {
      return "attr/" + (v.value0.value0 + (":" + v.value1));
    }
    ;
    if (v instanceof Attribute) {
      return "attr/:" + v.value1;
    }
    ;
    if (v instanceof Property2) {
      return "prop/" + v.value0;
    }
    ;
    if (v instanceof Handler) {
      return "handler/" + v.value0;
    }
    ;
    if (v instanceof Ref) {
      return "ref";
    }
    ;
    throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 182, column 16 - line 187, column 16): " + [v.constructor.name]);
  };
  var propFromInt = unsafeCoerce2;
  var buildProp = function(emit) {
    return function(el) {
      var removeProp = function(prevEvents) {
        return function(v, v1) {
          if (v1 instanceof Attribute) {
            return removeAttribute2(toNullable(v1.value0), v1.value1, el);
          }
          ;
          if (v1 instanceof Property2) {
            return removeProperty(v1.value0, el);
          }
          ;
          if (v1 instanceof Handler) {
            var handler3 = unsafeLookup(v1.value0, prevEvents);
            return removeEventListener(v1.value0, fst(handler3), el);
          }
          ;
          if (v1 instanceof Ref) {
            return unit;
          }
          ;
          throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 169, column 5 - line 179, column 18): " + [v1.constructor.name]);
        };
      };
      var mbEmit = function(v) {
        if (v instanceof Just) {
          return emit(v.value0)();
        }
        ;
        return unit;
      };
      var haltProp = function(state3) {
        var v = lookup5("ref")(state3.props);
        if (v instanceof Just && v.value0 instanceof Ref) {
          return mbEmit(v.value0.value0(new Removed(el)));
        }
        ;
        return unit;
      };
      var diffProp = function(prevEvents, events) {
        return function(v, v1, v11, v2) {
          if (v11 instanceof Attribute && v2 instanceof Attribute) {
            var $66 = v11.value2 === v2.value2;
            if ($66) {
              return v2;
            }
            ;
            setAttribute2(toNullable(v2.value0), v2.value1, v2.value2, el);
            return v2;
          }
          ;
          if (v11 instanceof Property2 && v2 instanceof Property2) {
            var v4 = refEq2(v11.value1, v2.value1);
            if (v4) {
              return v2;
            }
            ;
            if (v2.value0 === "value") {
              var elVal = unsafeGetProperty("value", el);
              var $75 = refEq2(elVal, v2.value1);
              if ($75) {
                return v2;
              }
              ;
              setProperty(v2.value0, v2.value1, el);
              return v2;
            }
            ;
            setProperty(v2.value0, v2.value1, el);
            return v2;
          }
          ;
          if (v11 instanceof Handler && v2 instanceof Handler) {
            var handler3 = unsafeLookup(v2.value0, prevEvents);
            write(v2.value1)(snd(handler3))();
            pokeMutMap(v2.value0, handler3, events);
            return v2;
          }
          ;
          return v2;
        };
      };
      var applyProp = function(events) {
        return function(v, v1, v2) {
          if (v2 instanceof Attribute) {
            setAttribute2(toNullable(v2.value0), v2.value1, v2.value2, el);
            return v2;
          }
          ;
          if (v2 instanceof Property2) {
            setProperty(v2.value0, v2.value1, el);
            return v2;
          }
          ;
          if (v2 instanceof Handler) {
            var v3 = unsafeGetAny(v2.value0, events);
            if (unsafeHasAny(v2.value0, events)) {
              write(v2.value1)(snd(v3))();
              return v2;
            }
            ;
            var ref3 = $$new(v2.value1)();
            var listener = eventListener(function(ev) {
              return function __do2() {
                var f$prime = read(ref3)();
                return mbEmit(f$prime(ev));
              };
            })();
            pokeMutMap(v2.value0, new Tuple(listener, ref3), events);
            addEventListener(v2.value0, listener, el);
            return v2;
          }
          ;
          if (v2 instanceof Ref) {
            mbEmit(v2.value0(new Created(el)));
            return v2;
          }
          ;
          throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 113, column 5 - line 135, column 15): " + [v2.constructor.name]);
        };
      };
      var $lazy_patchProp = $runtime_lazy6("patchProp", "Halogen.VDom.DOM.Prop", function() {
        return function(state3, ps2) {
          var events = newMutMap();
          var onThis = removeProp(state3.events);
          var onThese = diffProp(state3.events, events);
          var onThat = applyProp(events);
          var props = diffWithKeyAndIxE(state3.props, ps2, propToStrKey, onThese, onThis, onThat);
          var nextState = {
            events: unsafeFreeze2(events),
            props
          };
          return mkStep(new Step(unit, nextState, $lazy_patchProp(100), haltProp));
        };
      });
      var patchProp = $lazy_patchProp(87);
      var renderProp = function(ps1) {
        var events = newMutMap();
        var ps1$prime = strMapWithIxE(ps1, propToStrKey, applyProp(events));
        var state3 = {
          events: unsafeFreeze2(events),
          props: ps1$prime
        };
        return mkStep(new Step(unit, state3, patchProp, haltProp));
      };
      return renderProp;
    };
  };

  // output/Halogen.HTML.Core/index.js
  var HTML = function(x) {
    return x;
  };
  var widget = function($28) {
    return HTML(Widget.create($28));
  };
  var toPropValue = function(dict) {
    return dict.toPropValue;
  };
  var text = function($29) {
    return HTML(Text.create($29));
  };
  var ref = function(f) {
    return new Ref(function($30) {
      return f(function(v) {
        if (v instanceof Created) {
          return new Just(v.value0);
        }
        ;
        if (v instanceof Removed) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Halogen.HTML.Core (line 109, column 21 - line 111, column 23): " + [v.constructor.name]);
      }($30));
    });
  };
  var prop = function(dictIsProp) {
    var toPropValue1 = toPropValue(dictIsProp);
    return function(v) {
      var $31 = Property2.create(v);
      return function($32) {
        return $31(toPropValue1($32));
      };
    };
  };
  var isPropInt = {
    toPropValue: propFromInt
  };
  var handler = /* @__PURE__ */ function() {
    return Handler.create;
  }();
  var element = function(ns) {
    return function(name15) {
      return function(props) {
        return function(children2) {
          return new Elem(ns, name15, props, children2);
        };
      };
    };
  };
  var attr = function(ns) {
    return function(v) {
      return Attribute.create(ns)(v);
    };
  };

  // output/Control.Applicative.Free/index.js
  var identity8 = /* @__PURE__ */ identity(categoryFn);
  var Pure = /* @__PURE__ */ function() {
    function Pure2(value0) {
      this.value0 = value0;
    }
    ;
    Pure2.create = function(value0) {
      return new Pure2(value0);
    };
    return Pure2;
  }();
  var Lift2 = /* @__PURE__ */ function() {
    function Lift4(value0) {
      this.value0 = value0;
    }
    ;
    Lift4.create = function(value0) {
      return new Lift4(value0);
    };
    return Lift4;
  }();
  var Ap = /* @__PURE__ */ function() {
    function Ap2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Ap2.create = function(value0) {
      return function(value1) {
        return new Ap2(value0, value1);
      };
    };
    return Ap2;
  }();
  var mkAp = function(fba) {
    return function(fb) {
      return new Ap(fba, fb);
    };
  };
  var liftFreeAp = /* @__PURE__ */ function() {
    return Lift2.create;
  }();
  var goLeft = function(dictApplicative) {
    var pure10 = pure(dictApplicative);
    return function(fStack) {
      return function(valStack) {
        return function(nat) {
          return function(func) {
            return function(count) {
              if (func instanceof Pure) {
                return new Tuple(new Cons({
                  func: pure10(func.value0),
                  count
                }, fStack), valStack);
              }
              ;
              if (func instanceof Lift2) {
                return new Tuple(new Cons({
                  func: nat(func.value0),
                  count
                }, fStack), valStack);
              }
              ;
              if (func instanceof Ap) {
                return goLeft(dictApplicative)(fStack)(cons2(func.value1)(valStack))(nat)(func.value0)(count + 1 | 0);
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 102, column 41 - line 105, column 81): " + [func.constructor.name]);
            };
          };
        };
      };
    };
  };
  var goApply = function(dictApplicative) {
    var apply3 = apply(dictApplicative.Apply0());
    return function(fStack) {
      return function(vals) {
        return function(gVal) {
          if (fStack instanceof Nil) {
            return new Left(gVal);
          }
          ;
          if (fStack instanceof Cons) {
            var gRes = apply3(fStack.value0.func)(gVal);
            var $31 = fStack.value0.count === 1;
            if ($31) {
              if (fStack.value1 instanceof Nil) {
                return new Left(gRes);
              }
              ;
              return goApply(dictApplicative)(fStack.value1)(vals)(gRes);
            }
            ;
            if (vals instanceof Nil) {
              return new Left(gRes);
            }
            ;
            if (vals instanceof Cons) {
              return new Right(new Tuple(new Cons({
                func: gRes,
                count: fStack.value0.count - 1 | 0
              }, fStack.value1), new NonEmpty(vals.value0, vals.value1)));
            }
            ;
            throw new Error("Failed pattern match at Control.Applicative.Free (line 83, column 11 - line 88, column 50): " + [vals.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Control.Applicative.Free (line 72, column 3 - line 88, column 50): " + [fStack.constructor.name]);
        };
      };
    };
  };
  var functorFreeAp = {
    map: function(f) {
      return function(x) {
        return mkAp(new Pure(f))(x);
      };
    }
  };
  var foldFreeAp = function(dictApplicative) {
    var goApply1 = goApply(dictApplicative);
    var pure10 = pure(dictApplicative);
    var goLeft1 = goLeft(dictApplicative);
    return function(nat) {
      return function(z) {
        var go2 = function($copy_v) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(v) {
            if (v.value1.value0 instanceof Pure) {
              var v1 = goApply1(v.value0)(v.value1.value1)(pure10(v.value1.value0.value0));
              if (v1 instanceof Left) {
                $tco_done = true;
                return v1.value0;
              }
              ;
              if (v1 instanceof Right) {
                $copy_v = v1.value0;
                return;
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 54, column 17 - line 56, column 24): " + [v1.constructor.name]);
            }
            ;
            if (v.value1.value0 instanceof Lift2) {
              var v1 = goApply1(v.value0)(v.value1.value1)(nat(v.value1.value0.value0));
              if (v1 instanceof Left) {
                $tco_done = true;
                return v1.value0;
              }
              ;
              if (v1 instanceof Right) {
                $copy_v = v1.value0;
                return;
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 57, column 17 - line 59, column 24): " + [v1.constructor.name]);
            }
            ;
            if (v.value1.value0 instanceof Ap) {
              var nextVals = new NonEmpty(v.value1.value0.value1, v.value1.value1);
              $copy_v = goLeft1(v.value0)(nextVals)(nat)(v.value1.value0.value0)(1);
              return;
            }
            ;
            throw new Error("Failed pattern match at Control.Applicative.Free (line 53, column 5 - line 62, column 47): " + [v.value1.value0.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($copy_v);
          }
          ;
          return $tco_result;
        };
        return go2(new Tuple(Nil.value, singleton5(z)));
      };
    };
  };
  var retractFreeAp = function(dictApplicative) {
    return foldFreeAp(dictApplicative)(identity8);
  };
  var applyFreeAp = {
    apply: function(fba) {
      return function(fb) {
        return mkAp(fba)(fb);
      };
    },
    Functor0: function() {
      return functorFreeAp;
    }
  };
  var applicativeFreeAp = /* @__PURE__ */ function() {
    return {
      pure: Pure.create,
      Apply0: function() {
        return applyFreeAp;
      }
    };
  }();
  var foldFreeAp1 = /* @__PURE__ */ foldFreeAp(applicativeFreeAp);
  var hoistFreeAp = function(f) {
    return foldFreeAp1(function($54) {
      return liftFreeAp(f($54));
    });
  };

  // output/Halogen.Query.ChildQuery/index.js
  var ChildQuery = /* @__PURE__ */ function() {
    function ChildQuery3(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    ChildQuery3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new ChildQuery3(value0, value1, value22);
        };
      };
    };
    return ChildQuery3;
  }();
  var unChildQueryBox = unsafeCoerce2;
  var mkChildQueryBox = unsafeCoerce2;

  // output/Unsafe.Reference/foreign.js
  function reallyUnsafeRefEq(a2) {
    return function(b2) {
      return a2 === b2;
    };
  }

  // output/Unsafe.Reference/index.js
  var unsafeRefEq = reallyUnsafeRefEq;

  // output/Halogen.Subscription/index.js
  var $$void5 = /* @__PURE__ */ $$void(functorEffect);
  var bind5 = /* @__PURE__ */ bind(bindEffect);
  var append7 = /* @__PURE__ */ append(semigroupArray);
  var traverse_2 = /* @__PURE__ */ traverse_(applicativeEffect);
  var traverse_1 = /* @__PURE__ */ traverse_2(foldableArray);
  var unsubscribe = function(v) {
    return v;
  };
  var subscribe = function(v) {
    return function(k) {
      return v(function($76) {
        return $$void5(k($76));
      });
    };
  };
  var notify = function(v) {
    return function(a2) {
      return v(a2);
    };
  };
  var create = function __do() {
    var subscribers = $$new([])();
    return {
      emitter: function(k) {
        return function __do2() {
          modify_(function(v) {
            return append7(v)([k]);
          })(subscribers)();
          return modify_(deleteBy(unsafeRefEq)(k))(subscribers);
        };
      },
      listener: function(a2) {
        return bind5(read(subscribers))(traverse_1(function(k) {
          return k(a2);
        }));
      }
    };
  };

  // output/Halogen.Query.HalogenM/index.js
  var identity9 = /* @__PURE__ */ identity(categoryFn);
  var lookup6 = /* @__PURE__ */ lookup4();
  var SubscriptionId = function(x) {
    return x;
  };
  var ForkId = function(x) {
    return x;
  };
  var State = /* @__PURE__ */ function() {
    function State2(value0) {
      this.value0 = value0;
    }
    ;
    State2.create = function(value0) {
      return new State2(value0);
    };
    return State2;
  }();
  var Subscribe = /* @__PURE__ */ function() {
    function Subscribe2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Subscribe2.create = function(value0) {
      return function(value1) {
        return new Subscribe2(value0, value1);
      };
    };
    return Subscribe2;
  }();
  var Unsubscribe = /* @__PURE__ */ function() {
    function Unsubscribe2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Unsubscribe2.create = function(value0) {
      return function(value1) {
        return new Unsubscribe2(value0, value1);
      };
    };
    return Unsubscribe2;
  }();
  var Lift3 = /* @__PURE__ */ function() {
    function Lift4(value0) {
      this.value0 = value0;
    }
    ;
    Lift4.create = function(value0) {
      return new Lift4(value0);
    };
    return Lift4;
  }();
  var ChildQuery2 = /* @__PURE__ */ function() {
    function ChildQuery3(value0) {
      this.value0 = value0;
    }
    ;
    ChildQuery3.create = function(value0) {
      return new ChildQuery3(value0);
    };
    return ChildQuery3;
  }();
  var Raise = /* @__PURE__ */ function() {
    function Raise2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Raise2.create = function(value0) {
      return function(value1) {
        return new Raise2(value0, value1);
      };
    };
    return Raise2;
  }();
  var Par = /* @__PURE__ */ function() {
    function Par2(value0) {
      this.value0 = value0;
    }
    ;
    Par2.create = function(value0) {
      return new Par2(value0);
    };
    return Par2;
  }();
  var Fork = /* @__PURE__ */ function() {
    function Fork2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Fork2.create = function(value0) {
      return function(value1) {
        return new Fork2(value0, value1);
      };
    };
    return Fork2;
  }();
  var Join = /* @__PURE__ */ function() {
    function Join2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Join2.create = function(value0) {
      return function(value1) {
        return new Join2(value0, value1);
      };
    };
    return Join2;
  }();
  var Kill = /* @__PURE__ */ function() {
    function Kill2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Kill2.create = function(value0) {
      return function(value1) {
        return new Kill2(value0, value1);
      };
    };
    return Kill2;
  }();
  var GetRef = /* @__PURE__ */ function() {
    function GetRef2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    GetRef2.create = function(value0) {
      return function(value1) {
        return new GetRef2(value0, value1);
      };
    };
    return GetRef2;
  }();
  var HalogenM = function(x) {
    return x;
  };
  var raise = function(o) {
    return liftF(new Raise(o, unit));
  };
  var query = function() {
    return function(dictIsSymbol) {
      var lookup13 = lookup6(dictIsSymbol);
      return function(dictOrd) {
        var lookup23 = lookup13(dictOrd);
        return function(label5) {
          return function(p2) {
            return function(q2) {
              return liftF(new ChildQuery2(mkChildQueryBox(new ChildQuery(function(dictApplicative) {
                var pure13 = pure(dictApplicative);
                return function(k) {
                  var $177 = maybe(pure13(Nothing.value))(k);
                  var $178 = lookup23(label5)(p2);
                  return function($179) {
                    return $177($178($179));
                  };
                };
              }, q2, identity9))));
            };
          };
        };
      };
    };
  };
  var ordSubscriptionId = ordInt;
  var ordForkId = ordInt;
  var monadTransHalogenM = {
    lift: function(dictMonad) {
      return function($180) {
        return HalogenM(liftF(Lift3.create($180)));
      };
    }
  };
  var monadHalogenM = freeMonad;
  var monadStateHalogenM = {
    state: function($181) {
      return HalogenM(liftF(State.create($181)));
    },
    Monad0: function() {
      return monadHalogenM;
    }
  };
  var monadEffectHalogenM = function(dictMonadEffect) {
    return {
      liftEffect: function() {
        var $186 = liftEffect(dictMonadEffect);
        return function($187) {
          return HalogenM(liftF(Lift3.create($186($187))));
        };
      }(),
      Monad0: function() {
        return monadHalogenM;
      }
    };
  };
  var getRef = function(p2) {
    return liftF(new GetRef(p2, identity9));
  };
  var functorHalogenM = freeFunctor;
  var bindHalogenM = freeBind;
  var applicativeHalogenM = freeApplicative;

  // output/Halogen.Query.HalogenQ/index.js
  var Initialize = /* @__PURE__ */ function() {
    function Initialize4(value0) {
      this.value0 = value0;
    }
    ;
    Initialize4.create = function(value0) {
      return new Initialize4(value0);
    };
    return Initialize4;
  }();
  var Finalize = /* @__PURE__ */ function() {
    function Finalize2(value0) {
      this.value0 = value0;
    }
    ;
    Finalize2.create = function(value0) {
      return new Finalize2(value0);
    };
    return Finalize2;
  }();
  var Receive = /* @__PURE__ */ function() {
    function Receive2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Receive2.create = function(value0) {
      return function(value1) {
        return new Receive2(value0, value1);
      };
    };
    return Receive2;
  }();
  var Action2 = /* @__PURE__ */ function() {
    function Action3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Action3.create = function(value0) {
      return function(value1) {
        return new Action3(value0, value1);
      };
    };
    return Action3;
  }();
  var Query = /* @__PURE__ */ function() {
    function Query3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Query3.create = function(value0) {
      return function(value1) {
        return new Query3(value0, value1);
      };
    };
    return Query3;
  }();

  // output/Halogen.VDom.Thunk/index.js
  var $runtime_lazy7 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var unsafeEqThunk = function(v, v1) {
    return refEq2(v.value0, v1.value0) && (refEq2(v.value1, v1.value1) && v.value1(v.value3, v1.value3));
  };
  var runThunk = function(v) {
    return v.value2(v.value3);
  };
  var buildThunk = function(toVDom) {
    var haltThunk = function(state3) {
      return halt(state3.vdom);
    };
    var $lazy_patchThunk = $runtime_lazy7("patchThunk", "Halogen.VDom.Thunk", function() {
      return function(state3, t2) {
        var $48 = unsafeEqThunk(state3.thunk, t2);
        if ($48) {
          return mkStep(new Step(extract2(state3.vdom), state3, $lazy_patchThunk(112), haltThunk));
        }
        ;
        var vdom = step(state3.vdom, toVDom(runThunk(t2)));
        return mkStep(new Step(extract2(vdom), {
          vdom,
          thunk: t2
        }, $lazy_patchThunk(115), haltThunk));
      };
    });
    var patchThunk = $lazy_patchThunk(108);
    var renderThunk = function(spec) {
      return function(t) {
        var vdom = buildVDom(spec)(toVDom(runThunk(t)));
        return mkStep(new Step(extract2(vdom), {
          thunk: t,
          vdom
        }, patchThunk, haltThunk));
      };
    };
    return renderThunk;
  };

  // output/Halogen.Component/index.js
  var voidLeft2 = /* @__PURE__ */ voidLeft(functorHalogenM);
  var traverse_3 = /* @__PURE__ */ traverse_(applicativeHalogenM)(foldableMaybe);
  var map17 = /* @__PURE__ */ map(functorHalogenM);
  var pure5 = /* @__PURE__ */ pure(applicativeHalogenM);
  var lookup7 = /* @__PURE__ */ lookup4();
  var pop3 = /* @__PURE__ */ pop2();
  var insert4 = /* @__PURE__ */ insert3();
  var ComponentSlot = /* @__PURE__ */ function() {
    function ComponentSlot2(value0) {
      this.value0 = value0;
    }
    ;
    ComponentSlot2.create = function(value0) {
      return new ComponentSlot2(value0);
    };
    return ComponentSlot2;
  }();
  var ThunkSlot = /* @__PURE__ */ function() {
    function ThunkSlot2(value0) {
      this.value0 = value0;
    }
    ;
    ThunkSlot2.create = function(value0) {
      return new ThunkSlot2(value0);
    };
    return ThunkSlot2;
  }();
  var unComponentSlot = unsafeCoerce2;
  var unComponent = unsafeCoerce2;
  var mkEval = function(args) {
    return function(v) {
      if (v instanceof Initialize) {
        return voidLeft2(traverse_3(args.handleAction)(args.initialize))(v.value0);
      }
      ;
      if (v instanceof Finalize) {
        return voidLeft2(traverse_3(args.handleAction)(args.finalize))(v.value0);
      }
      ;
      if (v instanceof Receive) {
        return voidLeft2(traverse_3(args.handleAction)(args.receive(v.value0)))(v.value1);
      }
      ;
      if (v instanceof Action2) {
        return voidLeft2(args.handleAction(v.value0))(v.value1);
      }
      ;
      if (v instanceof Query) {
        return unCoyoneda(function(g) {
          var $45 = map17(maybe(v.value1(unit))(g));
          return function($46) {
            return $45(args.handleQuery($46));
          };
        })(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Halogen.Component (line 182, column 15 - line 192, column 71): " + [v.constructor.name]);
    };
  };
  var mkComponentSlot = unsafeCoerce2;
  var mkComponent = unsafeCoerce2;
  var defaultEval = /* @__PURE__ */ function() {
    return {
      handleAction: $$const(pure5(unit)),
      handleQuery: $$const(pure5(Nothing.value)),
      receive: $$const(Nothing.value),
      initialize: Nothing.value,
      finalize: Nothing.value
    };
  }();
  var componentSlot = function() {
    return function(dictIsSymbol) {
      var lookup13 = lookup7(dictIsSymbol);
      var pop12 = pop3(dictIsSymbol);
      var insert13 = insert4(dictIsSymbol);
      return function(dictOrd) {
        var lookup23 = lookup13(dictOrd);
        var pop22 = pop12(dictOrd);
        var insert22 = insert13(dictOrd);
        return function(label5) {
          return function(p2) {
            return function(comp) {
              return function(input3) {
                return function(output2) {
                  return mkComponentSlot({
                    get: lookup23(label5)(p2),
                    pop: pop22(label5)(p2),
                    set: insert22(label5)(p2),
                    component: comp,
                    input: input3,
                    output: output2
                  });
                };
              };
            };
          };
        };
      };
    };
  };

  // output/Halogen.HTML.Elements/index.js
  var element2 = /* @__PURE__ */ function() {
    return element(Nothing.value);
  }();
  var h3 = /* @__PURE__ */ element2("h3");
  var h3_ = /* @__PURE__ */ h3([]);
  var div2 = /* @__PURE__ */ element2("div");
  var div_ = /* @__PURE__ */ div2([]);
  var canvas = function(props) {
    return element2("canvas")(props)([]);
  };
  var button = /* @__PURE__ */ element2("button");

  // output/Halogen.HTML.Properties/index.js
  var ref2 = /* @__PURE__ */ function() {
    var go2 = function(p2) {
      return function(mel) {
        return new Just(new RefUpdate(p2, mel));
      };
    };
    return function($29) {
      return ref(go2($29));
    };
  }();
  var prop2 = function(dictIsProp) {
    return prop(dictIsProp);
  };
  var prop3 = /* @__PURE__ */ prop2(isPropInt);
  var width = /* @__PURE__ */ prop3("width");
  var height = /* @__PURE__ */ prop3("height");
  var attr2 = /* @__PURE__ */ function() {
    return attr(Nothing.value);
  }();

  // output/Halogen.Canvas/index.js
  var bind6 = /* @__PURE__ */ bind(bindHalogenM);
  var get2 = /* @__PURE__ */ get(monadStateHalogenM);
  var traverse2 = /* @__PURE__ */ traverse(traversableMaybe)(applicativeHalogenM);
  var lift6 = /* @__PURE__ */ lift(monadTransHalogenM);
  var traverse_4 = /* @__PURE__ */ traverse_(applicativeHalogenM)(foldableMaybe);
  var modify_3 = /* @__PURE__ */ modify_2(monadStateHalogenM);
  var Initialize2 = /* @__PURE__ */ function() {
    function Initialize4() {
    }
    ;
    Initialize4.value = new Initialize4();
    return Initialize4;
  }();
  var render = function(v) {
    return canvas([ref2("canvas"), width(v.dimensions.width), height(v.dimensions.height)]);
  };
  var handleQuery = function(dictMonadAff) {
    var lift1 = lift6(dictMonadAff.MonadEffect0().Monad0());
    var runCanvasT2 = runCanvasT(dictMonadAff);
    return function(dictMonadRec) {
      var runCanvasT1 = runCanvasT2(dictMonadRec);
      return function(f) {
        return bind6(get2)(function(v) {
          return flip(traverse2)(v.canvas)(function(c) {
            return lift1(runReaderT(runCanvasT1(f))(c));
          });
        });
      };
    };
  };
  var handleAction = function(dictMonadAff) {
    var liftEffect8 = liftEffect(monadEffectHalogenM(dictMonadAff.MonadEffect0()));
    return function(v) {
      return bind6(getRef("canvas"))(function(e) {
        return flip(traverse_4)(e)(function(ce) {
          return bind6(liftEffect8(getContext2D(ce)))(function(context2D) {
            return modify_3(function(st) {
              var $34 = {};
              for (var $35 in st) {
                if ({}.hasOwnProperty.call(st, $35)) {
                  $34[$35] = st[$35];
                }
                ;
              }
              ;
              $34.canvas = new Just({
                canvasElement: ce,
                context2D
              });
              return $34;
            });
          });
        });
      });
    };
  };
  var component = function(dictMonadAff) {
    var handleAction1 = handleAction(dictMonadAff);
    var handleQuery1 = handleQuery(dictMonadAff);
    return function(dictMonadRec) {
      return mkComponent({
        initialState: function(dimensions) {
          return {
            dimensions,
            canvas: Nothing.value
          };
        },
        render,
        "eval": mkEval({
          receive: defaultEval.receive,
          finalize: defaultEval.finalize,
          handleAction: handleAction1,
          handleQuery: handleQuery1(dictMonadRec),
          initialize: new Just(Initialize2.value)
        })
      });
    };
  };

  // output/Halogen.HTML/index.js
  var componentSlot2 = /* @__PURE__ */ componentSlot();
  var slot_ = function() {
    return function(dictIsSymbol) {
      var componentSlot1 = componentSlot2(dictIsSymbol);
      return function(dictOrd) {
        var componentSlot22 = componentSlot1(dictOrd);
        return function(label5) {
          return function(p2) {
            return function(component5) {
              return function(input3) {
                return widget(new ComponentSlot(componentSlot22(label5)(p2)(component5)(input3)($$const(Nothing.value))));
              };
            };
          };
        };
      };
    };
  };
  var slot = function() {
    return function(dictIsSymbol) {
      var componentSlot1 = componentSlot2(dictIsSymbol);
      return function(dictOrd) {
        var componentSlot22 = componentSlot1(dictOrd);
        return function(label5) {
          return function(p2) {
            return function(component5) {
              return function(input3) {
                return function(outputQuery) {
                  return widget(new ComponentSlot(componentSlot22(label5)(p2)(component5)(input3)(function($11) {
                    return Just.create(outputQuery($11));
                  })));
                };
              };
            };
          };
        };
      };
    };
  };

  // output/Web.Event.Event/foreign.js
  function stopPropagation(e) {
    return function() {
      return e.stopPropagation();
    };
  }

  // output/Web.HTML.Event.EventTypes/index.js
  var domcontentloaded = "DOMContentLoaded";

  // output/Web.UIEvent.KeyboardEvent.EventTypes/index.js
  var keyup = "keyup";
  var keydown = "keydown";

  // output/Web.UIEvent.MouseEvent.EventTypes/index.js
  var mouseup = "mouseup";
  var mouseover = "mouseover";
  var mousemove = "mousemove";
  var mouseleave = "mouseleave";
  var mouseenter = "mouseenter";
  var mousedown = "mousedown";
  var dblclick = "dblclick";
  var click = "click";

  // output/Halogen.HTML.Events/index.js
  var touchHandler = unsafeCoerce2;
  var mouseHandler = unsafeCoerce2;
  var keyHandler = unsafeCoerce2;
  var handler2 = function(et) {
    return function(f) {
      return handler(et)(function(ev) {
        return new Just(new Action(f(ev)));
      });
    };
  };
  var onClick = /* @__PURE__ */ function() {
    var $15 = handler2(click);
    return function($16) {
      return $15(mouseHandler($16));
    };
  }();
  var onDoubleClick = /* @__PURE__ */ function() {
    var $17 = handler2(dblclick);
    return function($18) {
      return $17(mouseHandler($18));
    };
  }();
  var onKeyDown = /* @__PURE__ */ function() {
    var $23 = handler2(keydown);
    return function($24) {
      return $23(keyHandler($24));
    };
  }();
  var onKeyUp = /* @__PURE__ */ function() {
    var $25 = handler2(keyup);
    return function($26) {
      return $25(keyHandler($26));
    };
  }();
  var onMouseDown = /* @__PURE__ */ function() {
    var $27 = handler2(mousedown);
    return function($28) {
      return $27(mouseHandler($28));
    };
  }();
  var onMouseEnter = /* @__PURE__ */ function() {
    var $29 = handler2(mouseenter);
    return function($30) {
      return $29(mouseHandler($30));
    };
  }();
  var onMouseLeave = /* @__PURE__ */ function() {
    var $31 = handler2(mouseleave);
    return function($32) {
      return $31(mouseHandler($32));
    };
  }();
  var onMouseMove = /* @__PURE__ */ function() {
    var $33 = handler2(mousemove);
    return function($34) {
      return $33(mouseHandler($34));
    };
  }();
  var onMouseOver = /* @__PURE__ */ function() {
    var $37 = handler2(mouseover);
    return function($38) {
      return $37(mouseHandler($38));
    };
  }();
  var onMouseUp = /* @__PURE__ */ function() {
    var $39 = handler2(mouseup);
    return function($40) {
      return $39(mouseHandler($40));
    };
  }();
  var onTouchCancel = /* @__PURE__ */ function() {
    var $41 = handler2("touchcancel");
    return function($42) {
      return $41(touchHandler($42));
    };
  }();
  var onTouchEnd = /* @__PURE__ */ function() {
    var $43 = handler2("touchend");
    return function($44) {
      return $43(touchHandler($44));
    };
  }();
  var onTouchEnter = /* @__PURE__ */ function() {
    var $45 = handler2("touchenter");
    return function($46) {
      return $45(touchHandler($46));
    };
  }();
  var onTouchLeave = /* @__PURE__ */ function() {
    var $47 = handler2("touchleave");
    return function($48) {
      return $47(touchHandler($48));
    };
  }();
  var onTouchMove = /* @__PURE__ */ function() {
    var $49 = handler2("touchmove");
    return function($50) {
      return $49(touchHandler($50));
    };
  }();
  var onTouchStart = /* @__PURE__ */ function() {
    var $51 = handler2("touchstart");
    return function($52) {
      return $51(touchHandler($52));
    };
  }();

  // output/Web.TouchEvent.TouchEvent/foreign.js
  function touches(e) {
    return e.touches;
  }

  // output/Web.TouchEvent.TouchEvent/index.js
  var toEvent = unsafeCoerce2;

  // output/Web.UIEvent.KeyboardEvent/index.js
  var toEvent2 = unsafeCoerce2;

  // output/Web.UIEvent.MouseEvent/foreign.js
  function clientX(e) {
    return e.clientX;
  }
  function clientY(e) {
    return e.clientY;
  }

  // output/Web.UIEvent.MouseEvent/index.js
  var toEvent3 = unsafeCoerce2;

  // output/Halogen.Canvas.Interact/index.js
  var discard3 = /* @__PURE__ */ discard(discardUnit)(bindHalogenM);
  var bind7 = /* @__PURE__ */ bind(bindHalogenM);
  var canvasIsSymbol = {
    reflectSymbol: function() {
      return "canvas";
    }
  };
  var query2 = /* @__PURE__ */ query()(canvasIsSymbol)(ordUnit);
  var traverse_5 = /* @__PURE__ */ traverse_(applicativeHalogenM)(foldableMaybe);
  var slot_2 = /* @__PURE__ */ slot_()(canvasIsSymbol)(ordUnit);
  var identity10 = /* @__PURE__ */ identity(categoryFn);
  var TouchCancel = /* @__PURE__ */ function() {
    function TouchCancel2(value0) {
      this.value0 = value0;
    }
    ;
    TouchCancel2.create = function(value0) {
      return new TouchCancel2(value0);
    };
    return TouchCancel2;
  }();
  var TouchEnd = /* @__PURE__ */ function() {
    function TouchEnd2(value0) {
      this.value0 = value0;
    }
    ;
    TouchEnd2.create = function(value0) {
      return new TouchEnd2(value0);
    };
    return TouchEnd2;
  }();
  var TouchEnter = /* @__PURE__ */ function() {
    function TouchEnter2(value0) {
      this.value0 = value0;
    }
    ;
    TouchEnter2.create = function(value0) {
      return new TouchEnter2(value0);
    };
    return TouchEnter2;
  }();
  var TouchLeave = /* @__PURE__ */ function() {
    function TouchLeave2(value0) {
      this.value0 = value0;
    }
    ;
    TouchLeave2.create = function(value0) {
      return new TouchLeave2(value0);
    };
    return TouchLeave2;
  }();
  var TouchMove = /* @__PURE__ */ function() {
    function TouchMove2(value0) {
      this.value0 = value0;
    }
    ;
    TouchMove2.create = function(value0) {
      return new TouchMove2(value0);
    };
    return TouchMove2;
  }();
  var TouchStart = /* @__PURE__ */ function() {
    function TouchStart2(value0) {
      this.value0 = value0;
    }
    ;
    TouchStart2.create = function(value0) {
      return new TouchStart2(value0);
    };
    return TouchStart2;
  }();
  var Click = /* @__PURE__ */ function() {
    function Click2(value0) {
      this.value0 = value0;
    }
    ;
    Click2.create = function(value0) {
      return new Click2(value0);
    };
    return Click2;
  }();
  var DoubleClick = /* @__PURE__ */ function() {
    function DoubleClick2(value0) {
      this.value0 = value0;
    }
    ;
    DoubleClick2.create = function(value0) {
      return new DoubleClick2(value0);
    };
    return DoubleClick2;
  }();
  var MouseDown = /* @__PURE__ */ function() {
    function MouseDown2(value0) {
      this.value0 = value0;
    }
    ;
    MouseDown2.create = function(value0) {
      return new MouseDown2(value0);
    };
    return MouseDown2;
  }();
  var MouseUp = /* @__PURE__ */ function() {
    function MouseUp2(value0) {
      this.value0 = value0;
    }
    ;
    MouseUp2.create = function(value0) {
      return new MouseUp2(value0);
    };
    return MouseUp2;
  }();
  var MouseEnter = /* @__PURE__ */ function() {
    function MouseEnter2(value0) {
      this.value0 = value0;
    }
    ;
    MouseEnter2.create = function(value0) {
      return new MouseEnter2(value0);
    };
    return MouseEnter2;
  }();
  var MouseLeave = /* @__PURE__ */ function() {
    function MouseLeave2(value0) {
      this.value0 = value0;
    }
    ;
    MouseLeave2.create = function(value0) {
      return new MouseLeave2(value0);
    };
    return MouseLeave2;
  }();
  var MouseMove = /* @__PURE__ */ function() {
    function MouseMove2(value0) {
      this.value0 = value0;
    }
    ;
    MouseMove2.create = function(value0) {
      return new MouseMove2(value0);
    };
    return MouseMove2;
  }();
  var KeyDown = /* @__PURE__ */ function() {
    function KeyDown2(value0) {
      this.value0 = value0;
    }
    ;
    KeyDown2.create = function(value0) {
      return new KeyDown2(value0);
    };
    return KeyDown2;
  }();
  var KeyUp = /* @__PURE__ */ function() {
    function KeyUp2(value0) {
      this.value0 = value0;
    }
    ;
    KeyUp2.create = function(value0) {
      return new KeyUp2(value0);
    };
    return KeyUp2;
  }();
  var KeyEvent = /* @__PURE__ */ function() {
    function KeyEvent2(value0) {
      this.value0 = value0;
    }
    ;
    KeyEvent2.create = function(value0) {
      return new KeyEvent2(value0);
    };
    return KeyEvent2;
  }();
  var MouseEvent = /* @__PURE__ */ function() {
    function MouseEvent2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    MouseEvent2.create = function(value0) {
      return function(value1) {
        return new MouseEvent2(value0, value1);
      };
    };
    return MouseEvent2;
  }();
  var TouchEvent = /* @__PURE__ */ function() {
    function TouchEvent2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TouchEvent2.create = function(value0) {
      return function(value1) {
        return new TouchEvent2(value0, value1);
      };
    };
    return TouchEvent2;
  }();
  var KeyInput = /* @__PURE__ */ function() {
    function KeyInput2(value0) {
      this.value0 = value0;
    }
    ;
    KeyInput2.create = function(value0) {
      return new KeyInput2(value0);
    };
    return KeyInput2;
  }();
  var MouseInput = /* @__PURE__ */ function() {
    function MouseInput2(value0) {
      this.value0 = value0;
    }
    ;
    MouseInput2.create = function(value0) {
      return new MouseInput2(value0);
    };
    return MouseInput2;
  }();
  var TouchInput = /* @__PURE__ */ function() {
    function TouchInput2(value0) {
      this.value0 = value0;
    }
    ;
    TouchInput2.create = function(value0) {
      return new TouchInput2(value0);
    };
    return TouchInput2;
  }();
  var isEventTouchInput = {
    toEvent: function(v) {
      if (v instanceof TouchCancel) {
        return toEvent(v.value0);
      }
      ;
      if (v instanceof TouchEnd) {
        return toEvent(v.value0);
      }
      ;
      if (v instanceof TouchEnter) {
        return toEvent(v.value0);
      }
      ;
      if (v instanceof TouchLeave) {
        return toEvent(v.value0);
      }
      ;
      if (v instanceof TouchMove) {
        return toEvent(v.value0);
      }
      ;
      if (v instanceof TouchStart) {
        return toEvent(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Halogen.Canvas.Interact (line 135, column 1 - line 141, column 48): " + [v.constructor.name]);
    }
  };
  var isEventMouseInput = {
    toEvent: function(v) {
      if (v instanceof Click) {
        return toEvent3(v.value0);
      }
      ;
      if (v instanceof DoubleClick) {
        return toEvent3(v.value0);
      }
      ;
      if (v instanceof MouseDown) {
        return toEvent3(v.value0);
      }
      ;
      if (v instanceof MouseUp) {
        return toEvent3(v.value0);
      }
      ;
      if (v instanceof MouseEnter) {
        return toEvent3(v.value0);
      }
      ;
      if (v instanceof MouseLeave) {
        return toEvent3(v.value0);
      }
      ;
      if (v instanceof MouseMove) {
        return toEvent3(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Halogen.Canvas.Interact (line 126, column 1 - line 133, column 47): " + [v.constructor.name]);
    }
  };
  var isEventKeyInput = {
    toEvent: function(v) {
      if (v instanceof KeyDown) {
        return toEvent2(v.value0);
      }
      ;
      if (v instanceof KeyUp) {
        return toEvent2(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Halogen.Canvas.Interact (line 122, column 1 - line 124, column 46): " + [v.constructor.name]);
    }
  };
  var toEvent4 = function(dict) {
    return dict.toEvent;
  };
  var stopInputEventPropagation = function(dictIsEvent) {
    var toEvent1 = toEvent4(dictIsEvent);
    return function(e) {
      return stopPropagation(toEvent1(e));
    };
  };
  var stopInputEventPropagation1 = /* @__PURE__ */ stopInputEventPropagation(isEventKeyInput);
  var stopInputEventPropagation2 = /* @__PURE__ */ stopInputEventPropagation(isEventMouseInput);
  var stopInputEventPropagation3 = /* @__PURE__ */ stopInputEventPropagation(isEventTouchInput);
  var _canvas = /* @__PURE__ */ function() {
    return $$Proxy.value;
  }();
  var handleAction2 = function(dictMonadAff) {
    var liftEffect8 = liftEffect(monadEffectHalogenM(dictMonadAff.MonadEffect0()));
    return function(dictMonadRec) {
      return function(v) {
        if (v instanceof KeyInput) {
          return discard3(liftEffect8(stopInputEventPropagation1(v.value0)))(function() {
            return raise(new KeyEvent(v.value0));
          });
        }
        ;
        if (v instanceof MouseInput) {
          return discard3(liftEffect8(stopInputEventPropagation2(v.value0)))(function() {
            return bind7(query2(_canvas)(unit)(getBoundingClientRect2))(function(rect2) {
              return flip(traverse_5)(rect2)(function(r) {
                return raise(new MouseEvent(v.value0, r));
              });
            });
          });
        }
        ;
        if (v instanceof TouchInput) {
          return discard3(liftEffect8(stopInputEventPropagation3(v.value0)))(function() {
            return bind7(query2(_canvas)(unit)(getBoundingClientRect2))(function(rect2) {
              return flip(traverse_5)(rect2)(function(r) {
                return raise(new TouchEvent(v.value0, r));
              });
            });
          });
        }
        ;
        throw new Error("Failed pattern match at Halogen.Canvas.Interact (line 103, column 16 - line 116, column 32): " + [v.constructor.name]);
      };
    };
  };
  var render2 = function(dictMonadAff) {
    var component1 = component(dictMonadAff);
    return function(dictMonadRec) {
      var component22 = component1(dictMonadRec);
      return function(dimensions) {
        return div2([onKeyDown(function($65) {
          return KeyInput.create(KeyDown.create($65));
        }), onKeyUp(function($66) {
          return KeyInput.create(KeyUp.create($66));
        }), onClick(function($67) {
          return MouseInput.create(Click.create($67));
        }), onDoubleClick(function($68) {
          return MouseInput.create(DoubleClick.create($68));
        }), onMouseDown(function($69) {
          return MouseInput.create(MouseDown.create($69));
        }), onMouseUp(function($70) {
          return MouseInput.create(MouseUp.create($70));
        }), onMouseEnter(function($71) {
          return MouseInput.create(MouseEnter.create($71));
        }), onMouseLeave(function($72) {
          return MouseInput.create(MouseLeave.create($72));
        }), onMouseMove(function($73) {
          return MouseInput.create(MouseMove.create($73));
        }), onTouchCancel(function($74) {
          return TouchInput.create(TouchCancel.create($74));
        }), onTouchEnd(function($75) {
          return TouchInput.create(TouchEnd.create($75));
        }), onTouchEnter(function($76) {
          return TouchInput.create(TouchEnter.create($76));
        }), onTouchLeave(function($77) {
          return TouchInput.create(TouchLeave.create($77));
        }), onTouchMove(function($78) {
          return TouchInput.create(TouchMove.create($78));
        }), onTouchStart(function($79) {
          return TouchInput.create(TouchStart.create($79));
        })])([slot_2(_canvas)(unit)(component22)(dimensions)]);
      };
    };
  };
  var component2 = function(dictMonadAff) {
    var render1 = render2(dictMonadAff);
    var handleAction1 = handleAction2(dictMonadAff);
    return function(dictMonadRec) {
      return mkComponent({
        initialState: identity10,
        render: render1(dictMonadRec),
        "eval": mkEval({
          receive: defaultEval.receive,
          initialize: defaultEval.initialize,
          finalize: defaultEval.finalize,
          handleAction: handleAction1(dictMonadRec),
          handleQuery: query2(_canvas)(unit)
        })
      });
    };
  };

  // output/Effect.Console/foreign.js
  var warn = function(s) {
    return function() {
      console.warn(s);
    };
  };

  // output/CSS.Render/index.js
  var map19 = /* @__PURE__ */ map(functorArray);
  var lookup8 = /* @__PURE__ */ lookup(foldableArray)(eqString);
  var collect$prime = function(v) {
    return function(v1) {
      if (v instanceof Plain && v1 instanceof Plain) {
        return [new Right(new Tuple(v.value0, v1.value0))];
      }
      ;
      if (v instanceof Prefixed && v1 instanceof Plain) {
        return map19(function(v3) {
          return new Right(new Tuple(v3.value0 + v3.value1, v1.value0));
        })(v.value0);
      }
      ;
      if (v instanceof Plain && v1 instanceof Prefixed) {
        return map19(function(v2) {
          return new Right(new Tuple(v.value0, v2.value0 + v2.value1));
        })(v1.value0);
      }
      ;
      if (v instanceof Prefixed && v1 instanceof Prefixed) {
        return map19(function(v2) {
          return maybe(new Left(v2.value0 + v2.value1))(function() {
            var $213 = Tuple.create(v2.value0 + v2.value1);
            return function($214) {
              return Right.create($213(function(v3) {
                return v2.value0 + v3;
              }($214)));
            };
          }())(lookup8(v2.value0)(v1.value0));
        })(v.value0);
      }
      ;
      throw new Error("Failed pattern match at CSS.Render (line 158, column 1 - line 158, column 80): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var collect2 = function(v) {
    return collect$prime(v.value0)(v.value1);
  };

  // output/Halogen.HTML.CSS/index.js
  var bind8 = /* @__PURE__ */ bind(bindArray);
  var fromFoldable4 = /* @__PURE__ */ fromFoldable3(foldableArray);
  var style2 = /* @__PURE__ */ function() {
    var toString = function() {
      var $13 = joinWith("; ");
      var $14 = foldMap2(monoidArray)(function(key5) {
        return function(val) {
          return [key5 + (": " + val)];
        };
      });
      return function($15) {
        return $13($14($15));
      };
    }();
    var rights = concatMap(foldMap(foldableEither)(monoidArray)(singleton2));
    var property = function(v) {
      if (v instanceof Property) {
        return new Just(new Tuple(v.value0, v.value1));
      }
      ;
      return Nothing.value;
    };
    var rules = function(rs) {
      var properties = bind8(mapMaybe(property)(rs))(function($16) {
        return rights(collect2($16));
      });
      return fromFoldable4(properties);
    };
    var $17 = attr2("style");
    return function($18) {
      return $17(toString(rules(runS($18))));
    };
  }();

  // output/Web.TouchEvent.Touch/foreign.js
  function clientX2(t) {
    return t.clientX;
  }
  function clientY2(t) {
    return t.clientY;
  }

  // output/Web.TouchEvent.TouchList/foreign.js
  function _item2(i2, l) {
    return l.item(i2);
  }

  // output/Web.TouchEvent.TouchList/index.js
  var item = function(i2) {
    return function(l) {
      return toMaybe(_item2(i2, l));
    };
  };

  // output/Examples.Halogen.Canvas.Sketch/index.js
  var discard4 = /* @__PURE__ */ discard(discardUnit);
  var discard12 = /* @__PURE__ */ discard4(bindStyleM);
  var when2 = /* @__PURE__ */ when(applicativeStyleM);
  var eq2 = /* @__PURE__ */ eq(/* @__PURE__ */ eqMaybe(eqInt));
  var show4 = /* @__PURE__ */ show(/* @__PURE__ */ showRecord()()(/* @__PURE__ */ showRecordFieldsCons({
    reflectSymbol: function() {
      return "x";
    }
  })(/* @__PURE__ */ showRecordFieldsConsNil({
    reflectSymbol: function() {
      return "y";
    }
  })(showNumber))(showNumber)));
  var map20 = /* @__PURE__ */ map(functorArray);
  var get3 = /* @__PURE__ */ get(monadStateHalogenM);
  var modify5 = /* @__PURE__ */ modify2(monadStateHalogenM);
  var map110 = /* @__PURE__ */ map(functorMaybe);
  var discard22 = /* @__PURE__ */ discard4(bindCanvasT);
  var $$void6 = /* @__PURE__ */ $$void(functorCanvasT);
  var traverseWithIndex2 = /* @__PURE__ */ traverseWithIndex(traversableWithIndexArray)(applicativeCanvasT);
  var void1 = /* @__PURE__ */ $$void(functorHalogenM);
  var sketchIsSymbol = {
    reflectSymbol: function() {
      return "sketch";
    }
  };
  var query3 = /* @__PURE__ */ query()(sketchIsSymbol)(ordUnit);
  var bind9 = /* @__PURE__ */ bind(bindCanvasT);
  var bind12 = /* @__PURE__ */ bind(bindHalogenM);
  var discard32 = /* @__PURE__ */ discard4(bindHalogenM);
  var modify_4 = /* @__PURE__ */ modify_2(monadStateHalogenM);
  var identity11 = /* @__PURE__ */ identity(categoryFn);
  var slot2 = /* @__PURE__ */ slot()(sketchIsSymbol)(ordUnit);
  var Initialize3 = /* @__PURE__ */ function() {
    function Initialize4() {
    }
    ;
    Initialize4.value = new Initialize4();
    return Initialize4;
  }();
  var InputEvent = /* @__PURE__ */ function() {
    function InputEvent2(value0) {
      this.value0 = value0;
    }
    ;
    InputEvent2.create = function(value0) {
      return new InputEvent2(value0);
    };
    return InputEvent2;
  }();
  var Select = /* @__PURE__ */ function() {
    function Select2(value0) {
      this.value0 = value0;
    }
    ;
    Select2.create = function(value0) {
      return new Select2(value0);
    };
    return Select2;
  }();
  var Delete = /* @__PURE__ */ function() {
    function Delete2(value0) {
      this.value0 = value0;
    }
    ;
    Delete2.create = function(value0) {
      return new Delete2(value0);
    };
    return Delete2;
  }();
  var renderLines = function(dictMonadAff) {
    return function(dictMonadRec) {
      return function(v) {
        var renderLine = function(v1) {
          return div2([style2(discard12(border(solid)(px(1))(black))(function() {
            return when2(eq2(new Just(v1.value0))(v.selected))(backgroundColor(rgb(180)(180)(180)));
          })), onMouseOver($$const(new Select(v1.value0))), onTouchStart($$const(new Select(v1.value0)))])([button([onClick($$const(new Delete(v1.value0)))])([text("delete")]), text(show4(v1.value1.value0) + (" " + show4(v1.value1.value1)))]);
        };
        return div_(map20(renderLine)(zip(range2(0)(length(v.lines)))(v.lines)));
      };
    };
  };
  var interact = function(v) {
    if (v instanceof KeyEvent) {
      return get3;
    }
    ;
    if (v instanceof MouseEvent) {
      if (v.value0 instanceof MouseLeave) {
        return modify5(function(w) {
          return {
            dimensions: w.dimensions,
            lines: w.lines,
            selected: w.selected,
            line: Nothing.value
          };
        });
      }
      ;
      if (v.value0 instanceof MouseUp) {
        return modify5(function(w) {
          return {
            dimensions: w.dimensions,
            selected: w.selected,
            line: Nothing.value,
            lines: function() {
              if (w.line instanceof Nothing) {
                return w.lines;
              }
              ;
              if (w.line instanceof Just) {
                return cons(w.line.value0)(w.lines);
              }
              ;
              throw new Error("Failed pattern match at Examples.Halogen.Canvas.Sketch (line 141, column 19 - line 143, column 42): " + [w.line.constructor.name]);
            }()
          };
        });
      }
      ;
      if (v.value0 instanceof MouseDown) {
        var p2 = {
          x: toNumber(clientX(v.value0.value0)) - v.value1.left,
          y: toNumber(clientY(v.value0.value0)) - v.value1.top
        };
        return modify5(function(w) {
          return {
            dimensions: w.dimensions,
            lines: w.lines,
            selected: w.selected,
            line: maybe(new Just(new Tuple(p2, p2)))(Just.create)(w.line)
          };
        });
      }
      ;
      if (v.value0 instanceof MouseMove) {
        var p2 = {
          x: toNumber(clientX(v.value0.value0)) - v.value1.left,
          y: toNumber(clientY(v.value0.value0)) - v.value1.top
        };
        var update = function(v12) {
          return new Tuple(v12.value0, p2);
        };
        return modify5(function(w) {
          return {
            dimensions: w.dimensions,
            lines: w.lines,
            selected: w.selected,
            line: map110(update)(w.line)
          };
        });
      }
      ;
      return get3;
    }
    ;
    if (v instanceof TouchEvent) {
      if (v.value0 instanceof TouchStart) {
        var v1 = item(0)(touches(v.value0.value0));
        if (v1 instanceof Nothing) {
          return get3;
        }
        ;
        if (v1 instanceof Just) {
          var p2 = {
            x: toNumber(clientX2(v1.value0)) - v.value1.left,
            y: toNumber(clientY2(v1.value0)) - v.value1.top
          };
          return modify5(function(w) {
            return {
              dimensions: w.dimensions,
              lines: w.lines,
              selected: w.selected,
              line: maybe(new Just(new Tuple(p2, p2)))(Just.create)(w.line)
            };
          });
        }
        ;
        throw new Error("Failed pattern match at Examples.Halogen.Canvas.Sketch (line 156, column 11 - line 160, column 78): " + [v1.constructor.name]);
      }
      ;
      if (v.value0 instanceof TouchMove) {
        var v1 = item(0)(touches(v.value0.value0));
        if (v1 instanceof Nothing) {
          return get3;
        }
        ;
        if (v1 instanceof Just) {
          var p2 = {
            x: toNumber(clientX2(v1.value0)) - v.value1.left,
            y: toNumber(clientY2(v1.value0)) - v.value1.top
          };
          var update = function(v2) {
            return new Tuple(v2.value0, p2);
          };
          return modify5(function(w) {
            return {
              dimensions: w.dimensions,
              lines: w.lines,
              selected: w.selected,
              line: map110(update)(w.line)
            };
          });
        }
        ;
        throw new Error("Failed pattern match at Examples.Halogen.Canvas.Sketch (line 162, column 11 - line 167, column 62): " + [v1.constructor.name]);
      }
      ;
      if (v.value0 instanceof TouchEnd) {
        return modify5(function(w) {
          return {
            dimensions: w.dimensions,
            selected: w.selected,
            line: Nothing.value,
            lines: function() {
              if (w.line instanceof Nothing) {
                return w.lines;
              }
              ;
              if (w.line instanceof Just) {
                return cons(w.line.value0)(w.lines);
              }
              ;
              throw new Error("Failed pattern match at Examples.Halogen.Canvas.Sketch (line 172, column 22 - line 174, column 45): " + [w.line.constructor.name]);
            }()
          };
        });
      }
      ;
      if (v.value0 instanceof TouchLeave) {
        return modify5(function(w) {
          return {
            dimensions: w.dimensions,
            lines: w.lines,
            selected: w.selected,
            line: Nothing.value
          };
        });
      }
      ;
      return get3;
    }
    ;
    throw new Error("Failed pattern match at Examples.Halogen.Canvas.Sketch (line 132, column 3 - line 177, column 19): " + [v.constructor.name]);
  };
  var draw = function(w) {
    return discard22(setFillColor(rgb(255)(255)(200)))(function() {
      return discard22(fillRect2({
        x: 0,
        y: 0,
        width: 400,
        height: 400
      }))(function() {
        return discard22(setLineWidth2(5))(function() {
          return $$void6(flip(traverseWithIndex2)(maybe(w.lines)(flip(cons)(w.lines))(w.line))(function(i2) {
            return function(v) {
              return discard22(beginPath2)(function() {
                return discard22(moveTo2(v.value0))(function() {
                  return discard22(lineTo2(v.value1))(function() {
                    return discard22(function() {
                      var $112 = eq2(new Just(i2))(w.selected);
                      if ($112) {
                        return setStrokeColor(rgb(255)(0)(0));
                      }
                      ;
                      return setStrokeColor(rgb(0)(0)(0));
                    }())(function() {
                      return stroke2;
                    });
                  });
                });
              });
            };
          }));
        });
      });
    });
  };
  var _sketch = /* @__PURE__ */ function() {
    return $$Proxy.value;
  }();
  var handleAction3 = /* @__PURE__ */ function() {
    var clearAndDraw = function(s) {
      return void1(query3(_sketch)(unit)(withContext(bind9(getWidth)(function(width9) {
        return bind9(getHeight)(function(height9) {
          return discard22(clearRect2({
            x: 0,
            y: 0,
            width: width9,
            height: height9
          }))(function() {
            return draw(s);
          });
        });
      }))));
    };
    return function(v) {
      if (v instanceof Initialize3) {
        return bind12(get3)(clearAndDraw);
      }
      ;
      if (v instanceof InputEvent) {
        return discard32(modify_4(function(st) {
          var $116 = {};
          for (var $117 in st) {
            if ({}.hasOwnProperty.call(st, $117)) {
              $116[$117] = st[$117];
            }
            ;
          }
          ;
          $116.selected = Nothing.value;
          return $116;
        }))(function() {
          return bind12(interact(v.value0))(function(s) {
            return clearAndDraw(s);
          });
        });
      }
      ;
      if (v instanceof Select) {
        return bind12(modify5(function(st) {
          return {
            dimensions: st.dimensions,
            line: st.line,
            lines: st.lines,
            selected: new Just(v.value0)
          };
        }))(function(s) {
          return clearAndDraw(s);
        });
      }
      ;
      if (v instanceof Delete) {
        return bind12(modify5(function(st) {
          return {
            dimensions: st.dimensions,
            line: st.line,
            selected: Nothing.value,
            lines: maybe(st.lines)(identity11)(deleteAt(v.value0)(st.lines))
          };
        }))(function(s) {
          return clearAndDraw(s);
        });
      }
      ;
      throw new Error("Failed pattern match at Examples.Halogen.Canvas.Sketch (line 89, column 3 - line 102, column 21): " + [v.constructor.name]);
    };
  }();
  var render4 = function(dictMonadAff) {
    var component1 = component2(dictMonadAff);
    var renderLines1 = renderLines(dictMonadAff);
    return function(dictMonadRec) {
      var component22 = component1(dictMonadRec);
      var renderLines2 = renderLines1(dictMonadRec);
      return function(v) {
        return div2([style2(discard12(display(flex))(function() {
          return flexDirection(row);
        }))])([slot2(_sketch)(unit)(component22)(v.dimensions)(InputEvent.create), div_([h3_([text("Lines")]), renderLines2(v)])]);
      };
    };
  };
  var component3 = function(dictMonadAff) {
    var render1 = render4(dictMonadAff);
    return function(dictMonadRec) {
      return mkComponent({
        initialState: $$const({
          dimensions: {
            width: 400,
            height: 400
          },
          line: Nothing.value,
          lines: [],
          selected: Nothing.value
        }),
        render: render1(dictMonadRec),
        "eval": mkEval({
          handleQuery: defaultEval.handleQuery,
          receive: defaultEval.receive,
          finalize: defaultEval.finalize,
          initialize: new Just(Initialize3.value),
          handleAction: handleAction3
        })
      });
    };
  };

  // output/Web.HTML/foreign.js
  var windowImpl = function() {
    return window;
  };

  // output/Web.HTML.HTMLDocument/foreign.js
  function _readyState(doc) {
    return doc.readyState;
  }

  // output/Web.HTML.HTMLDocument.ReadyState/index.js
  var Loading = /* @__PURE__ */ function() {
    function Loading2() {
    }
    ;
    Loading2.value = new Loading2();
    return Loading2;
  }();
  var Interactive = /* @__PURE__ */ function() {
    function Interactive2() {
    }
    ;
    Interactive2.value = new Interactive2();
    return Interactive2;
  }();
  var Complete = /* @__PURE__ */ function() {
    function Complete2() {
    }
    ;
    Complete2.value = new Complete2();
    return Complete2;
  }();
  var parse = function(v) {
    if (v === "loading") {
      return new Just(Loading.value);
    }
    ;
    if (v === "interactive") {
      return new Just(Interactive.value);
    }
    ;
    if (v === "complete") {
      return new Just(Complete.value);
    }
    ;
    return Nothing.value;
  };

  // output/Web.HTML.HTMLDocument/index.js
  var map21 = /* @__PURE__ */ map(functorEffect);
  var toParentNode = unsafeCoerce2;
  var toDocument = unsafeCoerce2;
  var readyState = function(doc) {
    return map21(function() {
      var $4 = fromMaybe(Loading.value);
      return function($5) {
        return $4(parse($5));
      };
    }())(function() {
      return _readyState(doc);
    });
  };

  // output/Web.HTML.HTMLElement/foreign.js
  function _read(nothing, just, value14) {
    var tag = Object.prototype.toString.call(value14);
    if (tag.indexOf("[object HTML") === 0 && tag.indexOf("Element]") === tag.length - 8) {
      return just(value14);
    } else {
      return nothing;
    }
  }

  // output/Web.HTML.HTMLElement/index.js
  var toNode2 = unsafeCoerce2;
  var fromElement = function(x) {
    return _read(Nothing.value, Just.create, x);
  };

  // output/Web.HTML.Window/foreign.js
  function document2(window2) {
    return function() {
      return window2.document;
    };
  }

  // output/Web.HTML.Window/index.js
  var toEventTarget = unsafeCoerce2;

  // output/Halogen.Aff.Util/index.js
  var bind10 = /* @__PURE__ */ bind(bindAff);
  var liftEffect4 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var bindFlipped4 = /* @__PURE__ */ bindFlipped(bindEffect);
  var composeKleisliFlipped2 = /* @__PURE__ */ composeKleisliFlipped(bindEffect);
  var pure6 = /* @__PURE__ */ pure(applicativeAff);
  var bindFlipped1 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var pure1 = /* @__PURE__ */ pure(applicativeEffect);
  var map22 = /* @__PURE__ */ map(functorEffect);
  var discard5 = /* @__PURE__ */ discard(discardUnit);
  var throwError2 = /* @__PURE__ */ throwError(monadThrowAff);
  var selectElement = function(query4) {
    return bind10(liftEffect4(bindFlipped4(composeKleisliFlipped2(function() {
      var $16 = querySelector(query4);
      return function($17) {
        return $16(toParentNode($17));
      };
    }())(document2))(windowImpl)))(function(mel) {
      return pure6(bindFlipped1(fromElement)(mel));
    });
  };
  var runHalogenAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure1(unit))));
  var awaitLoad = /* @__PURE__ */ makeAff(function(callback) {
    return function __do2() {
      var rs = bindFlipped4(readyState)(bindFlipped4(document2)(windowImpl))();
      if (rs instanceof Loading) {
        var et = map22(toEventTarget)(windowImpl)();
        var listener = eventListener(function(v) {
          return callback(new Right(unit));
        })();
        addEventListener2(domcontentloaded)(listener)(false)(et)();
        return effectCanceler(removeEventListener2(domcontentloaded)(listener)(false)(et));
      }
      ;
      callback(new Right(unit))();
      return nonCanceler;
    };
  });
  var awaitBody = /* @__PURE__ */ discard5(bindAff)(awaitLoad)(function() {
    return bind10(selectElement("body"))(function(body2) {
      return maybe(throwError2(error("Could not find body")))(pure6)(body2);
    });
  });

  // output/Control.Monad.Fork.Class/index.js
  var monadForkAff = {
    suspend: suspendAff,
    fork: forkAff,
    join: joinFiber,
    Monad0: function() {
      return monadAff;
    },
    Functor1: function() {
      return functorFiber;
    }
  };
  var fork = function(dict) {
    return dict.fork;
  };

  // output/Halogen.Aff.Driver.State/index.js
  var unRenderStateX = unsafeCoerce2;
  var unDriverStateX = unsafeCoerce2;
  var renderStateX_ = function(dictApplicative) {
    var traverse_9 = traverse_(dictApplicative)(foldableMaybe);
    return function(f) {
      return unDriverStateX(function(st) {
        return traverse_9(f)(st.rendering);
      });
    };
  };
  var mkRenderStateX = unsafeCoerce2;
  var renderStateX = function(dictFunctor) {
    return function(f) {
      return unDriverStateX(function(st) {
        return mkRenderStateX(f(st.rendering));
      });
    };
  };
  var mkDriverStateXRef = unsafeCoerce2;
  var mapDriverState = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var initDriverState = function(component5) {
    return function(input3) {
      return function(handler3) {
        return function(lchs) {
          return function __do2() {
            var selfRef = $$new({})();
            var childrenIn = $$new(empty5)();
            var childrenOut = $$new(empty5)();
            var handlerRef = $$new(handler3)();
            var pendingQueries = $$new(new Just(Nil.value))();
            var pendingOuts = $$new(new Just(Nil.value))();
            var pendingHandlers = $$new(Nothing.value)();
            var fresh2 = $$new(1)();
            var subscriptions = $$new(new Just(empty4))();
            var forks = $$new(empty4)();
            var ds = {
              component: component5,
              state: component5.initialState(input3),
              refs: empty4,
              children: empty5,
              childrenIn,
              childrenOut,
              selfRef,
              handlerRef,
              pendingQueries,
              pendingOuts,
              pendingHandlers,
              rendering: Nothing.value,
              fresh: fresh2,
              subscriptions,
              forks,
              lifecycleHandlers: lchs
            };
            write(ds)(selfRef)();
            return mkDriverStateXRef(selfRef);
          };
        };
      };
    };
  };

  // output/Halogen.Aff.Driver.Eval/index.js
  var traverse_6 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var bindFlipped5 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var lookup9 = /* @__PURE__ */ lookup3(ordSubscriptionId);
  var bind13 = /* @__PURE__ */ bind(bindAff);
  var liftEffect5 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var discard6 = /* @__PURE__ */ discard(discardUnit);
  var discard13 = /* @__PURE__ */ discard6(bindAff);
  var traverse_12 = /* @__PURE__ */ traverse_(applicativeAff);
  var traverse_22 = /* @__PURE__ */ traverse_12(foldableList);
  var fork3 = /* @__PURE__ */ fork(monadForkAff);
  var parSequence_3 = /* @__PURE__ */ parSequence_(parallelAff)(applicativeParAff)(foldableList);
  var pure7 = /* @__PURE__ */ pure(applicativeAff);
  var map23 = /* @__PURE__ */ map(functorCoyoneda);
  var parallel3 = /* @__PURE__ */ parallel(parallelAff);
  var map111 = /* @__PURE__ */ map(functorAff);
  var sequential2 = /* @__PURE__ */ sequential(parallelAff);
  var map24 = /* @__PURE__ */ map(functorMaybe);
  var insert5 = /* @__PURE__ */ insert2(ordSubscriptionId);
  var retractFreeAp2 = /* @__PURE__ */ retractFreeAp(applicativeParAff);
  var $$delete3 = /* @__PURE__ */ $$delete2(ordForkId);
  var unlessM2 = /* @__PURE__ */ unlessM(monadEffect);
  var insert12 = /* @__PURE__ */ insert2(ordForkId);
  var traverse_32 = /* @__PURE__ */ traverse_12(foldableMaybe);
  var lookup12 = /* @__PURE__ */ lookup3(ordForkId);
  var lookup22 = /* @__PURE__ */ lookup3(ordString);
  var foldFree2 = /* @__PURE__ */ foldFree(monadRecAff);
  var alter2 = /* @__PURE__ */ alter(ordString);
  var unsubscribe3 = function(sid) {
    return function(ref3) {
      return function __do2() {
        var v = read(ref3)();
        var subs = read(v.subscriptions)();
        return traverse_6(unsubscribe)(bindFlipped5(lookup9(sid))(subs))();
      };
    };
  };
  var queueOrRun = function(ref3) {
    return function(au) {
      return bind13(liftEffect5(read(ref3)))(function(v) {
        if (v instanceof Nothing) {
          return au;
        }
        ;
        if (v instanceof Just) {
          return liftEffect5(write(new Just(new Cons(au, v.value0)))(ref3));
        }
        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 188, column 33 - line 190, column 57): " + [v.constructor.name]);
      });
    };
  };
  var handleLifecycle = function(lchs) {
    return function(f) {
      return discard13(liftEffect5(write({
        initializers: Nil.value,
        finalizers: Nil.value
      })(lchs)))(function() {
        return bind13(liftEffect5(f))(function(result) {
          return bind13(liftEffect5(read(lchs)))(function(v) {
            return discard13(traverse_22(fork3)(v.finalizers))(function() {
              return discard13(parSequence_3(v.initializers))(function() {
                return pure7(result);
              });
            });
          });
        });
      });
    };
  };
  var handleAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure(applicativeEffect)(unit))));
  var fresh = function(f) {
    return function(ref3) {
      return bind13(liftEffect5(read(ref3)))(function(v) {
        return liftEffect5(modify$prime(function(i2) {
          return {
            state: i2 + 1 | 0,
            value: f(i2)
          };
        })(v.fresh));
      });
    };
  };
  var evalQ = function(render5) {
    return function(ref3) {
      return function(q2) {
        return bind13(liftEffect5(read(ref3)))(function(v) {
          return evalM(render5)(ref3)(v["component"]["eval"](new Query(map23(Just.create)(liftCoyoneda(q2)), $$const(Nothing.value))));
        });
      };
    };
  };
  var evalM = function(render5) {
    return function(initRef) {
      return function(v) {
        var evalChildQuery = function(ref3) {
          return function(cqb) {
            return bind13(liftEffect5(read(ref3)))(function(v1) {
              return unChildQueryBox(function(v2) {
                var evalChild = function(v3) {
                  return parallel3(bind13(liftEffect5(read(v3)))(function(dsx) {
                    return unDriverStateX(function(ds) {
                      return evalQ(render5)(ds.selfRef)(v2.value1);
                    })(dsx);
                  }));
                };
                return map111(v2.value2)(sequential2(v2.value0(applicativeParAff)(evalChild)(v1.children)));
              })(cqb);
            });
          };
        };
        var go2 = function(ref3) {
          return function(v1) {
            if (v1 instanceof State) {
              return bind13(liftEffect5(read(ref3)))(function(v2) {
                var v3 = v1.value0(v2.state);
                if (unsafeRefEq(v2.state)(v3.value1)) {
                  return pure7(v3.value0);
                }
                ;
                if (otherwise) {
                  return discard13(liftEffect5(write({
                    component: v2.component,
                    refs: v2.refs,
                    children: v2.children,
                    childrenIn: v2.childrenIn,
                    childrenOut: v2.childrenOut,
                    selfRef: v2.selfRef,
                    handlerRef: v2.handlerRef,
                    pendingQueries: v2.pendingQueries,
                    pendingOuts: v2.pendingOuts,
                    pendingHandlers: v2.pendingHandlers,
                    rendering: v2.rendering,
                    fresh: v2.fresh,
                    subscriptions: v2.subscriptions,
                    forks: v2.forks,
                    lifecycleHandlers: v2.lifecycleHandlers,
                    state: v3.value1
                  })(ref3)))(function() {
                    return discard13(handleLifecycle(v2.lifecycleHandlers)(render5(v2.lifecycleHandlers)(ref3)))(function() {
                      return pure7(v3.value0);
                    });
                  });
                }
                ;
                throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 86, column 7 - line 92, column 21): " + [v3.constructor.name]);
              });
            }
            ;
            if (v1 instanceof Subscribe) {
              return bind13(fresh(SubscriptionId)(ref3))(function(sid) {
                return bind13(liftEffect5(subscribe(v1.value0(sid))(function(act) {
                  return handleAff(evalF(render5)(ref3)(new Action(act)));
                })))(function(finalize) {
                  return bind13(liftEffect5(read(ref3)))(function(v2) {
                    return discard13(liftEffect5(modify_(map24(insert5(sid)(finalize)))(v2.subscriptions)))(function() {
                      return pure7(v1.value1(sid));
                    });
                  });
                });
              });
            }
            ;
            if (v1 instanceof Unsubscribe) {
              return discard13(liftEffect5(unsubscribe3(v1.value0)(ref3)))(function() {
                return pure7(v1.value1);
              });
            }
            ;
            if (v1 instanceof Lift3) {
              return v1.value0;
            }
            ;
            if (v1 instanceof ChildQuery2) {
              return evalChildQuery(ref3)(v1.value0);
            }
            ;
            if (v1 instanceof Raise) {
              return bind13(liftEffect5(read(ref3)))(function(v2) {
                return bind13(liftEffect5(read(v2.handlerRef)))(function(handler3) {
                  return discard13(queueOrRun(v2.pendingOuts)(handler3(v1.value0)))(function() {
                    return pure7(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Par) {
              return sequential2(retractFreeAp2(hoistFreeAp(function() {
                var $119 = evalM(render5)(ref3);
                return function($120) {
                  return parallel3($119($120));
                };
              }())(v1.value0)));
            }
            ;
            if (v1 instanceof Fork) {
              return bind13(fresh(ForkId)(ref3))(function(fid) {
                return bind13(liftEffect5(read(ref3)))(function(v2) {
                  return bind13(liftEffect5($$new(false)))(function(doneRef) {
                    return bind13(fork3($$finally(liftEffect5(function __do2() {
                      modify_($$delete3(fid))(v2.forks)();
                      return write(true)(doneRef)();
                    }))(evalM(render5)(ref3)(v1.value0))))(function(fiber) {
                      return discard13(liftEffect5(unlessM2(read(doneRef))(modify_(insert12(fid)(fiber))(v2.forks))))(function() {
                        return pure7(v1.value1(fid));
                      });
                    });
                  });
                });
              });
            }
            ;
            if (v1 instanceof Join) {
              return bind13(liftEffect5(read(ref3)))(function(v2) {
                return bind13(liftEffect5(read(v2.forks)))(function(forkMap) {
                  return discard13(traverse_32(joinFiber)(lookup12(v1.value0)(forkMap)))(function() {
                    return pure7(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Kill) {
              return bind13(liftEffect5(read(ref3)))(function(v2) {
                return bind13(liftEffect5(read(v2.forks)))(function(forkMap) {
                  return discard13(traverse_32(killFiber(error("Cancelled")))(lookup12(v1.value0)(forkMap)))(function() {
                    return pure7(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof GetRef) {
              return bind13(liftEffect5(read(ref3)))(function(v2) {
                return pure7(v1.value1(lookup22(v1.value0)(v2.refs)));
              });
            }
            ;
            throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 83, column 12 - line 139, column 33): " + [v1.constructor.name]);
          };
        };
        return foldFree2(go2(initRef))(v);
      };
    };
  };
  var evalF = function(render5) {
    return function(ref3) {
      return function(v) {
        if (v instanceof RefUpdate) {
          return liftEffect5(flip(modify_)(ref3)(mapDriverState(function(st) {
            return {
              component: st.component,
              state: st.state,
              children: st.children,
              childrenIn: st.childrenIn,
              childrenOut: st.childrenOut,
              selfRef: st.selfRef,
              handlerRef: st.handlerRef,
              pendingQueries: st.pendingQueries,
              pendingOuts: st.pendingOuts,
              pendingHandlers: st.pendingHandlers,
              rendering: st.rendering,
              fresh: st.fresh,
              subscriptions: st.subscriptions,
              forks: st.forks,
              lifecycleHandlers: st.lifecycleHandlers,
              refs: alter2($$const(v.value1))(v.value0)(st.refs)
            };
          })));
        }
        ;
        if (v instanceof Action) {
          return bind13(liftEffect5(read(ref3)))(function(v1) {
            return evalM(render5)(ref3)(v1["component"]["eval"](new Action2(v.value0, unit)));
          });
        }
        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 52, column 20 - line 58, column 62): " + [v.constructor.name]);
      };
    };
  };

  // output/Halogen.Aff.Driver/index.js
  var bind11 = /* @__PURE__ */ bind(bindEffect);
  var discard7 = /* @__PURE__ */ discard(discardUnit);
  var for_2 = /* @__PURE__ */ for_(applicativeEffect)(foldableMaybe);
  var traverse_7 = /* @__PURE__ */ traverse_(applicativeAff)(foldableList);
  var fork4 = /* @__PURE__ */ fork(monadForkAff);
  var bindFlipped6 = /* @__PURE__ */ bindFlipped(bindEffect);
  var traverse_13 = /* @__PURE__ */ traverse_(applicativeEffect);
  var traverse_23 = /* @__PURE__ */ traverse_13(foldableMaybe);
  var traverse_33 = /* @__PURE__ */ traverse_13(foldableMap);
  var discard23 = /* @__PURE__ */ discard7(bindAff);
  var parSequence_4 = /* @__PURE__ */ parSequence_(parallelAff)(applicativeParAff)(foldableList);
  var liftEffect6 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var pure8 = /* @__PURE__ */ pure(applicativeEffect);
  var map25 = /* @__PURE__ */ map(functorEffect);
  var pure12 = /* @__PURE__ */ pure(applicativeAff);
  var when3 = /* @__PURE__ */ when(applicativeEffect);
  var renderStateX2 = /* @__PURE__ */ renderStateX(functorEffect);
  var $$void7 = /* @__PURE__ */ $$void(functorAff);
  var foreachSlot2 = /* @__PURE__ */ foreachSlot(applicativeEffect);
  var renderStateX_2 = /* @__PURE__ */ renderStateX_(applicativeEffect);
  var tailRecM3 = /* @__PURE__ */ tailRecM(monadRecEffect);
  var voidLeft3 = /* @__PURE__ */ voidLeft(functorEffect);
  var bind14 = /* @__PURE__ */ bind(bindAff);
  var liftEffect1 = /* @__PURE__ */ liftEffect(monadEffectEffect);
  var newLifecycleHandlers = /* @__PURE__ */ function() {
    return $$new({
      initializers: Nil.value,
      finalizers: Nil.value
    });
  }();
  var handlePending = function(ref3) {
    return function __do2() {
      var queue = read(ref3)();
      write(Nothing.value)(ref3)();
      return for_2(queue)(function() {
        var $59 = traverse_7(fork4);
        return function($60) {
          return handleAff($59(reverse2($60)));
        };
      }())();
    };
  };
  var cleanupSubscriptionsAndForks = function(v) {
    return function __do2() {
      bindFlipped6(traverse_23(traverse_33(unsubscribe)))(read(v.subscriptions))();
      write(Nothing.value)(v.subscriptions)();
      bindFlipped6(traverse_33(function() {
        var $61 = killFiber(error("finalized"));
        return function($62) {
          return handleAff($61($62));
        };
      }()))(read(v.forks))();
      return write(empty4)(v.forks)();
    };
  };
  var runUI = function(renderSpec2) {
    return function(component5) {
      return function(i2) {
        var squashChildInitializers = function(lchs) {
          return function(preInits) {
            return unDriverStateX(function(st) {
              var parentInitializer = evalM(render5)(st.selfRef)(st["component"]["eval"](new Initialize(unit)));
              return modify_(function(handlers) {
                return {
                  initializers: new Cons(discard23(parSequence_4(reverse2(handlers.initializers)))(function() {
                    return discard23(parentInitializer)(function() {
                      return liftEffect6(function __do2() {
                        handlePending(st.pendingQueries)();
                        return handlePending(st.pendingOuts)();
                      });
                    });
                  }), preInits),
                  finalizers: handlers.finalizers
                };
              })(lchs);
            });
          };
        };
        var runComponent = function(lchs) {
          return function(handler3) {
            return function(j) {
              return unComponent(function(c) {
                return function __do2() {
                  var lchs$prime = newLifecycleHandlers();
                  var $$var2 = initDriverState(c)(j)(handler3)(lchs$prime)();
                  var pre2 = read(lchs)();
                  write({
                    initializers: Nil.value,
                    finalizers: pre2.finalizers
                  })(lchs)();
                  bindFlipped6(unDriverStateX(function() {
                    var $63 = render5(lchs);
                    return function($64) {
                      return $63(function(v) {
                        return v.selfRef;
                      }($64));
                    };
                  }()))(read($$var2))();
                  bindFlipped6(squashChildInitializers(lchs)(pre2.initializers))(read($$var2))();
                  return $$var2;
                };
              });
            };
          };
        };
        var renderChild = function(lchs) {
          return function(handler3) {
            return function(childrenInRef) {
              return function(childrenOutRef) {
                return unComponentSlot(function(slot3) {
                  return function __do2() {
                    var childrenIn = map25(slot3.pop)(read(childrenInRef))();
                    var $$var2 = function() {
                      if (childrenIn instanceof Just) {
                        write(childrenIn.value0.value1)(childrenInRef)();
                        var dsx = read(childrenIn.value0.value0)();
                        unDriverStateX(function(st) {
                          return function __do3() {
                            flip(write)(st.handlerRef)(function() {
                              var $65 = maybe(pure12(unit))(handler3);
                              return function($66) {
                                return $65(slot3.output($66));
                              };
                            }())();
                            return handleAff(evalM(render5)(st.selfRef)(st["component"]["eval"](new Receive(slot3.input, unit))))();
                          };
                        })(dsx)();
                        return childrenIn.value0.value0;
                      }
                      ;
                      if (childrenIn instanceof Nothing) {
                        return runComponent(lchs)(function() {
                          var $67 = maybe(pure12(unit))(handler3);
                          return function($68) {
                            return $67(slot3.output($68));
                          };
                        }())(slot3.input)(slot3.component)();
                      }
                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 213, column 14 - line 222, column 98): " + [childrenIn.constructor.name]);
                    }();
                    var isDuplicate = map25(function($69) {
                      return isJust(slot3.get($69));
                    })(read(childrenOutRef))();
                    when3(isDuplicate)(warn("Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"))();
                    modify_(slot3.set($$var2))(childrenOutRef)();
                    return bind11(read($$var2))(renderStateX2(function(v) {
                      if (v instanceof Nothing) {
                        return $$throw("Halogen internal error: child was not initialized in renderChild");
                      }
                      ;
                      if (v instanceof Just) {
                        return pure8(renderSpec2.renderChild(v.value0));
                      }
                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 227, column 37 - line 229, column 50): " + [v.constructor.name]);
                    }))();
                  };
                });
              };
            };
          };
        };
        var render5 = function(lchs) {
          return function($$var2) {
            return function __do2() {
              var v = read($$var2)();
              var shouldProcessHandlers = map25(isNothing)(read(v.pendingHandlers))();
              when3(shouldProcessHandlers)(write(new Just(Nil.value))(v.pendingHandlers))();
              write(empty5)(v.childrenOut)();
              write(v.children)(v.childrenIn)();
              var handler3 = function() {
                var $70 = queueOrRun(v.pendingHandlers);
                var $71 = evalF(render5)(v.selfRef);
                return function($72) {
                  return $70($$void7($71($72)));
                };
              }();
              var childHandler = function() {
                var $73 = queueOrRun(v.pendingQueries);
                return function($74) {
                  return $73(handler3(Action.create($74)));
                };
              }();
              var rendering = renderSpec2.render(function($75) {
                return handleAff(handler3($75));
              })(renderChild(lchs)(childHandler)(v.childrenIn)(v.childrenOut))(v.component.render(v.state))(v.rendering)();
              var children2 = read(v.childrenOut)();
              var childrenIn = read(v.childrenIn)();
              foreachSlot2(childrenIn)(function(v1) {
                return function __do3() {
                  var childDS = read(v1)();
                  renderStateX_2(renderSpec2.removeChild)(childDS)();
                  return finalize(lchs)(childDS)();
                };
              })();
              flip(modify_)(v.selfRef)(mapDriverState(function(ds$prime) {
                return {
                  component: ds$prime.component,
                  state: ds$prime.state,
                  refs: ds$prime.refs,
                  childrenIn: ds$prime.childrenIn,
                  childrenOut: ds$prime.childrenOut,
                  selfRef: ds$prime.selfRef,
                  handlerRef: ds$prime.handlerRef,
                  pendingQueries: ds$prime.pendingQueries,
                  pendingOuts: ds$prime.pendingOuts,
                  pendingHandlers: ds$prime.pendingHandlers,
                  fresh: ds$prime.fresh,
                  subscriptions: ds$prime.subscriptions,
                  forks: ds$prime.forks,
                  lifecycleHandlers: ds$prime.lifecycleHandlers,
                  rendering: new Just(rendering),
                  children: children2
                };
              }))();
              return when3(shouldProcessHandlers)(flip(tailRecM3)(unit)(function(v1) {
                return function __do3() {
                  var handlers = read(v.pendingHandlers)();
                  write(new Just(Nil.value))(v.pendingHandlers)();
                  traverse_23(function() {
                    var $76 = traverse_7(fork4);
                    return function($77) {
                      return handleAff($76(reverse2($77)));
                    };
                  }())(handlers)();
                  var mmore = read(v.pendingHandlers)();
                  var $52 = maybe(false)($$null)(mmore);
                  if ($52) {
                    return voidLeft3(write(Nothing.value)(v.pendingHandlers))(new Done(unit))();
                  }
                  ;
                  return new Loop(unit);
                };
              }))();
            };
          };
        };
        var finalize = function(lchs) {
          return unDriverStateX(function(st) {
            return function __do2() {
              cleanupSubscriptionsAndForks(st)();
              var f = evalM(render5)(st.selfRef)(st["component"]["eval"](new Finalize(unit)));
              modify_(function(handlers) {
                return {
                  initializers: handlers.initializers,
                  finalizers: new Cons(f, handlers.finalizers)
                };
              })(lchs)();
              return foreachSlot2(st.children)(function(v) {
                return function __do3() {
                  var dsx = read(v)();
                  return finalize(lchs)(dsx)();
                };
              })();
            };
          });
        };
        var evalDriver = function(disposed) {
          return function(ref3) {
            return function(q2) {
              return bind14(liftEffect6(read(disposed)))(function(v) {
                if (v) {
                  return pure12(Nothing.value);
                }
                ;
                return evalQ(render5)(ref3)(q2);
              });
            };
          };
        };
        var dispose = function(disposed) {
          return function(lchs) {
            return function(dsx) {
              return handleLifecycle(lchs)(function __do2() {
                var v = read(disposed)();
                if (v) {
                  return unit;
                }
                ;
                write(true)(disposed)();
                finalize(lchs)(dsx)();
                return unDriverStateX(function(v1) {
                  return function __do3() {
                    var v2 = liftEffect1(read(v1.selfRef))();
                    return for_2(v2.rendering)(renderSpec2.dispose)();
                  };
                })(dsx)();
              });
            };
          };
        };
        return bind14(liftEffect6(newLifecycleHandlers))(function(lchs) {
          return bind14(liftEffect6($$new(false)))(function(disposed) {
            return handleLifecycle(lchs)(function __do2() {
              var sio = create();
              var dsx = bindFlipped6(read)(runComponent(lchs)(function() {
                var $78 = notify(sio.listener);
                return function($79) {
                  return liftEffect6($78($79));
                };
              }())(i2)(component5))();
              return unDriverStateX(function(st) {
                return pure8({
                  query: evalDriver(disposed)(st.selfRef),
                  messages: sio.emitter,
                  dispose: dispose(disposed)(lchs)(dsx)
                });
              })(dsx)();
            });
          });
        });
      };
    };
  };

  // output/Web.DOM.Node/foreign.js
  var getEffProp2 = function(name15) {
    return function(node) {
      return function() {
        return node[name15];
      };
    };
  };
  var baseURI = getEffProp2("baseURI");
  var _ownerDocument = getEffProp2("ownerDocument");
  var _parentNode = getEffProp2("parentNode");
  var _parentElement = getEffProp2("parentElement");
  var childNodes = getEffProp2("childNodes");
  var _firstChild = getEffProp2("firstChild");
  var _lastChild = getEffProp2("lastChild");
  var _previousSibling = getEffProp2("previousSibling");
  var _nextSibling = getEffProp2("nextSibling");
  var _nodeValue = getEffProp2("nodeValue");
  var textContent = getEffProp2("textContent");
  function insertBefore(node1) {
    return function(node2) {
      return function(parent2) {
        return function() {
          parent2.insertBefore(node1, node2);
        };
      };
    };
  }
  function appendChild(node) {
    return function(parent2) {
      return function() {
        parent2.appendChild(node);
      };
    };
  }
  function removeChild2(node) {
    return function(parent2) {
      return function() {
        parent2.removeChild(node);
      };
    };
  }

  // output/Web.DOM.Node/index.js
  var map26 = /* @__PURE__ */ map(functorEffect);
  var parentNode2 = /* @__PURE__ */ function() {
    var $6 = map26(toMaybe);
    return function($7) {
      return $6(_parentNode($7));
    };
  }();
  var nextSibling = /* @__PURE__ */ function() {
    var $15 = map26(toMaybe);
    return function($16) {
      return $15(_nextSibling($16));
    };
  }();

  // output/Halogen.VDom.Driver/index.js
  var $runtime_lazy8 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var $$void8 = /* @__PURE__ */ $$void(functorEffect);
  var pure9 = /* @__PURE__ */ pure(applicativeEffect);
  var traverse_8 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var unwrap5 = /* @__PURE__ */ unwrap();
  var when4 = /* @__PURE__ */ when(applicativeEffect);
  var not2 = /* @__PURE__ */ not(/* @__PURE__ */ heytingAlgebraFunction(/* @__PURE__ */ heytingAlgebraFunction(heytingAlgebraBoolean)));
  var identity12 = /* @__PURE__ */ identity(categoryFn);
  var bind15 = /* @__PURE__ */ bind(bindAff);
  var liftEffect7 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var map27 = /* @__PURE__ */ map(functorEffect);
  var bindFlipped7 = /* @__PURE__ */ bindFlipped(bindEffect);
  var substInParent = function(v) {
    return function(v1) {
      return function(v2) {
        if (v1 instanceof Just && v2 instanceof Just) {
          return $$void8(insertBefore(v)(v1.value0)(v2.value0));
        }
        ;
        if (v1 instanceof Nothing && v2 instanceof Just) {
          return $$void8(appendChild(v)(v2.value0));
        }
        ;
        return pure9(unit);
      };
    };
  };
  var removeChild3 = function(v) {
    return function __do2() {
      var npn = parentNode2(v.node)();
      return traverse_8(function(pn) {
        return removeChild2(v.node)(pn);
      })(npn)();
    };
  };
  var mkSpec = function(handler3) {
    return function(renderChildRef) {
      return function(document3) {
        var getNode = unRenderStateX(function(v) {
          return v.node;
        });
        var done = function(st) {
          if (st instanceof Just) {
            return halt(st.value0);
          }
          ;
          return unit;
        };
        var buildWidget2 = function(spec) {
          var buildThunk2 = buildThunk(unwrap5)(spec);
          var $lazy_patch = $runtime_lazy8("patch", "Halogen.VDom.Driver", function() {
            return function(st, slot3) {
              if (st instanceof Just) {
                if (slot3 instanceof ComponentSlot) {
                  halt(st.value0);
                  return $lazy_renderComponentSlot(100)(slot3.value0);
                }
                ;
                if (slot3 instanceof ThunkSlot) {
                  var step$prime = step(st.value0, slot3.value0);
                  return mkStep(new Step(extract2(step$prime), new Just(step$prime), $lazy_patch(103), done));
                }
                ;
                throw new Error("Failed pattern match at Halogen.VDom.Driver (line 97, column 22 - line 103, column 79): " + [slot3.constructor.name]);
              }
              ;
              return $lazy_render(104)(slot3);
            };
          });
          var $lazy_render = $runtime_lazy8("render", "Halogen.VDom.Driver", function() {
            return function(slot3) {
              if (slot3 instanceof ComponentSlot) {
                return $lazy_renderComponentSlot(86)(slot3.value0);
              }
              ;
              if (slot3 instanceof ThunkSlot) {
                var step3 = buildThunk2(slot3.value0);
                return mkStep(new Step(extract2(step3), new Just(step3), $lazy_patch(89), done));
              }
              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 84, column 7 - line 89, column 75): " + [slot3.constructor.name]);
            };
          });
          var $lazy_renderComponentSlot = $runtime_lazy8("renderComponentSlot", "Halogen.VDom.Driver", function() {
            return function(cs) {
              var renderChild = read(renderChildRef)();
              var rsx = renderChild(cs)();
              var node = getNode(rsx);
              return mkStep(new Step(node, Nothing.value, $lazy_patch(117), done));
            };
          });
          var patch = $lazy_patch(91);
          var render5 = $lazy_render(82);
          var renderComponentSlot = $lazy_renderComponentSlot(109);
          return render5;
        };
        var buildAttributes = buildProp(handler3);
        return {
          buildWidget: buildWidget2,
          buildAttributes,
          document: document3
        };
      };
    };
  };
  var renderSpec = function(document3) {
    return function(container) {
      var render5 = function(handler3) {
        return function(child2) {
          return function(v) {
            return function(v1) {
              if (v1 instanceof Nothing) {
                return function __do2() {
                  var renderChildRef = $$new(child2)();
                  var spec = mkSpec(handler3)(renderChildRef)(document3);
                  var machine = buildVDom(spec)(v);
                  var node = extract2(machine);
                  $$void8(appendChild(node)(toNode2(container)))();
                  return {
                    machine,
                    node,
                    renderChildRef
                  };
                };
              }
              ;
              if (v1 instanceof Just) {
                return function __do2() {
                  write(child2)(v1.value0.renderChildRef)();
                  var parent2 = parentNode2(v1.value0.node)();
                  var nextSib = nextSibling(v1.value0.node)();
                  var machine$prime = step(v1.value0.machine, v);
                  var newNode = extract2(machine$prime);
                  when4(not2(unsafeRefEq)(v1.value0.node)(newNode))(substInParent(newNode)(nextSib)(parent2))();
                  return {
                    machine: machine$prime,
                    node: newNode,
                    renderChildRef: v1.value0.renderChildRef
                  };
                };
              }
              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 157, column 5 - line 173, column 80): " + [v1.constructor.name]);
            };
          };
        };
      };
      return {
        render: render5,
        renderChild: identity12,
        removeChild: removeChild3,
        dispose: removeChild3
      };
    };
  };
  var runUI2 = function(component5) {
    return function(i2) {
      return function(element4) {
        return bind15(liftEffect7(map27(toDocument)(bindFlipped7(document2)(windowImpl))))(function(document3) {
          return runUI(renderSpec(document3)(element4))(component5)(i2);
        });
      };
    };
  };

  // output/Main/index.js
  var $$void9 = /* @__PURE__ */ $$void(functorAff);
  var component4 = /* @__PURE__ */ component3(monadAffAff)(monadRecAff);
  var main2 = /* @__PURE__ */ runHalogenAff(/* @__PURE__ */ bind(bindAff)(awaitBody)(function(body2) {
    return $$void9(runUI2(component4)(unit)(body2));
  }));

  // <stdin>
  main2();
})();
