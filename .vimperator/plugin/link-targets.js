/*
 * Adds indicators to links based on where they lead.
 *
 * @author Kris Maglione <maglione.k at Gmail>
 * @version 0.1
 */

(function() {

const icons = {
    aim:
        "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAACaElEQVQ4jZWST0iTARjGn++bdqlE" +
        "0fVNU1OW21zuM61Asz+ucGVIhZPsUEK5i5SUXcKDrMAOjmUmui5lUKPWocus4aEpikKSJGgHAyFC" +
        "0KZL1vgG6WxPt8nICb3X931/PM/zvsA2FYlEaLfbWVhYSL1eT6fTye3m/6n29nZWH86h/9kF+lx1" +
        "PGjIosPhSICkJFsOBoMsLZXxofc4DLIEiCJ2CMDNxwMJc2IygCAIUKlUEEUBIIEYIYoiyP9w0dTU" +
        "xKpSDT++buDEy3pWmCR2d3cnEJIqGBkZ4afJMeTlpuH8DR9O2d5Do61ENBqFy+XaXsbq6ipl0wEO" +
        "Pqolv93mU3s1S4qyWFFWwEbLfubnari4uEggSYjT09PYKQRRZ64CA7/Q3GhA88UiQBCA1BTUNHvh" +
        "8XiQFDAzM4MjxVlAbB1/NjbQP/AVokhAEDD2OYDlyG5IkpQcMD8/D31+GqBEENsAwpE1OF58QXqG" +
        "Gl1dXbBYLFCr1cKWIU5NTXHI54VZzsC6oiA19hsdl/Ziot8MbbYKs7Oz8eU4QFEUhkIhut1u1lsb" +
        "cKchB8WZUSyFY+j1BbAQjELOieHVXSP8756jr68v8Qo9PT00Gg08UZ7NN/ePcs1rpjJYwyvniijL" +
        "Mk26PXz74Bg5dJqTT06yIE/i3Nzc5hV0Oh2Cy0vw9lZAm7mGhZ/APc93/Fjfh+FhD8bHx9Ha2orQ" +
        "VQ2un9Gg0rgLTqdzU0E4HKbVamXHtRI+vFVOWa+hzWbjyspKXKrf76ekTufls1oeKjPR7XYn2vB6" +
        "vdRqtWxpaeHo6OiWn9bZ2cm2tjYGAoF4/y/JfRaSQfvoKwAAAABJRU5ErkJggg",
    java:
        "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAuUlEQVQ4jZ1SQQ7DIAwz1V7T456w" +
        "Y4+8J0fva+FZ6WEFpaHQapYiIQN2YgDuYUddYnlwGSQf+ASQrM7TDtKd+935yxEO98hNvK7djaTx" +
       "PMZwlE5AVc0AMyCKjOGDM1f0HYXxfAZtw6w38pwXeVVCRJDz1g59SUAEAPBRbXzOG9b1XQ1TXbQW" +
        "Z0AfqCESM6hqJ7CQ/O+rHlhEpPtdpRSklE7lAvSG3V3/VP4pWkaBP3/lqiy/9L36aI0d80KynYgL" +
        "q8EAAAAASUVORK5CYII",
    excel:
        "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAAAXNSR0IArs4c6QAAAARnQU1BAACx" +
        "jwv8YQUAAAAgY0hSTQAAeiYAAICEAAD6AAAAgOgAAHUwAADqYAAAOpgAABdwnLpRPAAAAHJJREFU" +
        "OE+1klEOABEMRHt4ce1dm5FR0wb7wYdEM7w+WKnFqp3P9iv9nYuzn4OBZCe0vEdxO1sdARJEoyV8" +
        "hcuJIBxh9qV3gEy8BnSoDqjGPfTJCQQKJ3cQy41DfGzvsyfE3hIHeW5h5re0+CIT4eJvfQH6Qoee" +
        "QrXgXwAAAABJRU5ErkJggg",
    external:
        "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAA7EAAAOxAGVKw4bAAAA" +
        "gUlEQVQ4jdWSwQ2AIAxFf42jOBtO0CXoBDqbu+DBQKQUKUf/qSG8l9ICOHMAyTpfvIKehJjZNOts" +
        "IqXeAcr16oV0J1lSBDHG6tJJhK9kiTmDEfyWNAIvDDyzmNqChgHHGkNKCKlelGsLGrLgbgc9+GJu" +
        "ztwzsGBg4ifmiMj4Cf/KDXtEJE9FLbd2AAAAAElFTkSuQmCC",
    hand:
        "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAA7EAAAOxAGVKw4bAAAA" +
        "Z0lEQVQ4jcWT4QrAIAiET9n7v/LtxwxcWqT7sYNAivs0MyCKto4ks5l8vCKSnQfpaabfAL4fabwF" +
        "kIT1hKv4csSWdCK2AKk8cBVvAa0KOtdQAGJT15J3slLBGHV979UrCZ+p6v/8Cjfb1zn1mTBYdAAA" +
        "AABJRU5ErkJggg",
    insecure:
        "iVBORw0KGgoAAAANSUhEUgAAABUAAAAPCAYAAAALWoRrAAAACXBIWXMAAA7EAAAOxAGVKw4bAAAB" +
        "2ElEQVQ4jbWSvWtTURjGf/fmJqHmxiEpbqKDguAkOESE4iIIHQRxFRfnukud2n9AVxcHNxFEcAh0" +
        "sWC1H9jqELVgrEZMzEdJQpvk5tzzvg651YrY3gye5T0fnN/7nOc88B+GM+6FhcUVHZiQxo8myZSH" +
        "n83iqHL1ytTYLACeFRe1XKlprdnR3d5Atyo1bWx39dPXmj549FTHVrrwYlkzvk+73WF17S1uwkFF" +
        "McZSKJzj2GSejXclbt245nhxoaWPZc6eOcXSqzXmZmf+EDM7d08vTV2gud0BwI0LNcayvLrxFxBg" +
        "/u5t5/XKOp47wsVW2qi3GASDf55/3qqQz+WAyNPn94+qNV1EQAQ0qmJH1e7bw/W5Ob9z4F94ANZ0" +
        "mZ4p4nrnoz4KCGD3zQWxH3h45/Khr/IgUmCr4NWQ4UsIq6gIigA+3pECuHkIvyP2UOZvqJgybvo0" +
        "yDdUqkhokKCPSkgi7eNwkmH3DSoxoSpgg028zEV02CTsVbBBgFgLTpLUcBMmEgTt99i4UBHo1b+Q" +
        "ygnuxAmS6QxJDUENaAgagDSwg9bIqlhQC9Z0QHdxvOM4Ogn0gT6qPdA+aJ0waMX31ElkKT4poY+v" +
        "/4rVXqQ0itTemoQP7BwI/QlgKQSw6bxnzwAAAABJRU5ErkJggg",
    internal:
        "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAAAXNSR0IArs4c6QAAAARnQU1BAACx" +
        "jwv8YQUAAAAgY0hSTQAAeiYAAICEAAD6AAAAgOgAAHUwAADqYAAAOpgAABdwnLpRPAAAAAlwSFlz" +
        "AAAOxAAADsQBlSsOGwAAAEJJREFUOE9jbGhoYCAJADWQBBggqv8TASAqERpmMjDgQUADqaQB02kQ" +
        "a3HaMCI14IoKasQDwcSBEtPEpz9oWiJeAwCT6abhEwvS8AAAAABJRU5ErkJggg",
    javascript:
        "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAAAXNSR0IArs4c6QAAAARnQU1BAACx" +
        "jwv8YQUAAAAgY0hSTQAAeiYAAICEAAD6AAAAgOgAAHUwAADqYAAAOpgAABdwnLpRPAAAAAlwSFlz" +
        "AAAOxAAADsQBlSsOGwAAAIhJREFUOE+tUlsOwCAIczf3aNxsA+oQGSF8jOgytbxarjnnaJuAed+n" +
        "8Y03eyQiiRwc9DgUJF+A4JM4wFnTygICNScOLtV+5ktLEjOE8J6IvKSi+rqH1YD18DdLUEDJkeWZ" +
        "zZsOVLIaOUsvpybl0q4SDpSzoST8m8y50t8h9FO2e0CwjrXH2gEf6kuuBl00BtIAAAAASUVORK5C" +
        "YII",
    mailto:
        "iVBORw0KGgoAAAANSUhEUgAAABAAAAALCAIAAAD5gJpuAAAAAXNSR0IArs4c6QAAAARnQU1BAACx" +
        "jwv8YQUAAAAgY0hSTQAAeiYAAICEAAD6AAAAgOgAAHUwAADqYAAAOpgAABdwnLpRPAAAAAlwSFlz" +
        "AAAOxAAADsQBlSsOGwAAADRJREFUKFNjYCAD/CcIGBj+gxEUEFQPUU0NDTCD4CaCGMiAaCfBNKFr" +
        "oL0NBIOLXD+QFNcA58NKxEWp89MAAAAASUVORK5CYII",
    movie:
        "iVBORw0KGgoAAAANSUhEUgAAABAAAAAPCAYAAADtc08vAAAACXBIWXMAAA7DAAAOwwHHb6hkAAAA" +
        "vElEQVR4nJ2TwbHEIAxDXYpLUSkuRaXkRB06Uw0d6B/IJiF7+ewwGtDYfoAZIiK8K8c9IiJs21vz" +
        "EwBwa3eAK6BKzoQBukqukgE6E4v/FFdpBZA3gJTJWfD28cj5AgDw7lUutT6uo/3S0LWJJ+Tfmvk3" +
        "DZQj4QBnsDTXiW9PzfyFeOgKxnGKZ9HTJ27/HJCcgPOQoamgnMXFfwDQC9D6cAKm5NaHWx8TVlx8" +
        "YgJaHyvgpya+Psb+M74Bu/oDfC636egQFQ8AAAAASUVORK5CYII",
    msi:
        "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAA7DAAAOwwHHb6hkAAAA" +
        "uElEQVR4nJWS0RGEIBBDU5qlUEpKuS/ryLc1XBF2kPvQxQXRU5jMMEDeblAY40nS+upSJA1cmANQ" +
        "ijwNZPs5oEiel9XzsrpInj5/ACQdKuVsxh2ApGNkQDYPI+SqJC3JOUI2nwB9VUkVEOuRhh0AaC7c" +
        "SVILsN0AAJz0GEDSsI1tt0bMBS4BYZbUAEadGcbx2+6H+fGwQ/to+dPfApo2915eAVyjvOggIDE2" +
        "UNxuzRXQP0y/15/l+QO+4Gn+sUCJ0gAAAABJRU5ErkJggg",
    msword:
        "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAAAXNSR0IArs4c6QAAAARnQU1BAACx" +
        "jwv8YQUAAAAgY0hSTQAAeiYAAICEAAD6AAAAgOgAAHUwAADqYAAAOpgAABdwnLpRPAAAAAlwSFlz" +
        "AAAOxAAADsQBlSsOGwAAAG9JREFUOE9jYACB/ySRpKkGG83wnxgAdwXIBuI0QNxCoR+AVsHDAJmN" +
        "ZC7Yhpa2Fog6sCJouOFkI6vDagPQOCRxWCjhiY1jx4+h2IbmEkydkDCEiWPYgOZXiHvw2YAsBzQV" +
        "4h4SbIDH6UDFNMGUAgBo9iREr7+8vwAAAABJRU5ErkJggg",
    "new":
        "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAALHRFWHRDcmVhdGlvbiBUaW1lAFdl" +
        "ZCAyNSBBdWcgMjAwNCAxMzoxMzo1MyAtMDYwMAxWF1cAAAAHdElNRQfUCBkSDxTa+hjhAAAACXBI" +
        "WXMAAAsSAAALEgHS3X78AAAABGdBTUEAALGPC/xhBQAAAFxJREFUeNpjYKAQMGITnMnA8J8YzelA" +
        "/UyUuoCFkA2EXEgdFzQ0NKD6uaGBAZs4kI/hIuqGAcwGmB9hfAwX0swF6AAe2tAwwZY+aOMC9PiH" +
        "hQFdYoGoPEBVF1AMABE5HddSGZY7AAAAAElFTkSuQmCC",
    nofollow:
        "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAA7EAAAOxAGVKw4bAAAA" +
        "lklEQVQ4ja1TUQ7FIAhrzQ603f8Q7ka8j6kPm0aXl0dCMiktDJEwVoHQ2AXQ5U5BR9wJjYOSNTHj" +
        "GeOqsmk7qmBlQ3Cik5VV666ziMCVsEOU3a9YEfKpU3Kw+xlfThUs5wOIIUBy8mwu3r45BNp02V1m" +
        "MVUn2fNRVpPP2N2qd7K9xjebWOVsr2y1hYr/7y3shN5s6U/2AfYvRe9W5oCvAAAAAElFTkSuQmCC",
    email:
        "iVBORw0KGgoAAAANSUhEUgAAAA8AAAAPCAYAAAA71pVKAAABqElEQVQokc2Sy2qTURSFv/+WJk1/" +
        "06RRCkKgmtQKWryB6KAKTnwD30MfwIFTR/oEvoETKRWl1IpC8dJGwVkdtBQbEWljmpzLPttBLcnQ" +
        "ma7JhsX+WGuw4F8pAvjy5rEeGUEVkYD3Ae8F5w6v9wGRQNDD19t37kfpEXRyOiOfuQWEP46OZCjI" +
        "Hsg3Vp484sWnCgApgPOB8ek5tpYfUms1KderQy4IQQTfP+D94ktON0+x+G4HgBjA+0BSrFA/c55O" +
        "+wO97x3QgJoBwRqk3+Xzyirnbl5hcrKEExnC1gloAlnOxPE6uxvrdLe+IqaL7e2z9myJqamMWBVE" +
        "ccYNa1vr2d/dZPvja2Yv1inXUrY32vR+dvjRVeZvXCPPE94+f8XZ2RbOyxD2ts9me43mfIu0UiAt" +
        "GZrXc8QZsiyCNEeTjEsLBZaerhPEjdT2MY0LC0zUZ8AKaEo8doy0XEULVTROQZXx0hhXLzdw4ofJ" +
        "ALUTDezOKn5gUH+AWoeKJzhPBAQxqBjyYoKO1gZ4cO8uRfaQwS+8U4xE9G1CAKIQYQSMj3Fese5v" +
        "N/hf6jfRydcBy3hcmQAAAABJRU5ErkJggg",
    pdf:
        "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAABp1BMVEX//wC8vLweHh4lJSVAQEDC" +
        "wsLFxcXs7Oz29vb+/v7S0tK/v7/Q0NDb29vkcHDgb3DMzMzq6urkjI7hXV/u4eTSZGjXtbjj7O/v" +
        "/v7u7/Hu2dvpu73iWlry8vLx8fHw8PDag4XsUlLmR0fmSEnoqanoS0zq0tXp6eno6Oj19fXpqarl" +
        "jo/o7PDpfHv09PSxsbHm5ubt7e3nmZzcOzrn2trZ2dnj4+O2trbg4ODNzc3k5OTi4eLeTk3z8/Pc" +
        "3NzU1NS7u7vR0dHX19ff39/b2trSUVLd3d3Pz8/Ly8vT09PGxsbBwcHHx8e6urrExMTKysrl5eWA" +
        "BAbWGRzY2NiLi41+foBycnR0dHeJiYyKAALaAAPtAAP5AAP+BAj+Cw/+Ehf+Gh7+Jyr+YWPr6+uu" +
        "rq5xcXOqAQToAAT0AAP+AAT+Bgr+DBH+FRn+IST+Njn+dHarq6v6+vqAgITaGR36LjL+MTT+Nzr+" +
        "PT/+QUT+SEz+U1b+amz9fH/h4eHe3t6Hh4ynp6uxsbe5ub+7u8G9vcXCwsrDw8Pn5+eysrKvr6+w" +
        "sLC0tLTa2tqQTo13AAAAAXRSTlMAQObYZgAAAOVJREFUGJVjYGBg6Onu6uzq7OzsaGeAgLbWluam" +
        "xob6us5aIK+muqqyorystKS4qLCuACiSn5ebk52VmZGellpYWJeSzJCUmBAfFxsTHRUZER4WGhLM" +
        "EBQJBYEB/n6+Pt4gQ708PdzdXF2cnRy9gh3AAs72drY21slWPZYW5mABM9NkE2MjW0MrTwN9sICB" +
        "hZ6ujraWZrupBkQgWV1NVUVFWUlRQR4iICcrIy0lKSEuJqoOERARFpIVNLByNhWwgwjw88mbWfA6" +
        "8bhzc4EF/Dg52NUDanmdHNm4OkECrCzMUMDEwggA5uo65lIWsBgAAAAASUVORK5CYII",
    powerpoint:
        "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAAAXNSR0IArs4c6QAAAARnQU1BAACx" +
        "jwv8YQUAAAAgY0hSTQAAeiYAAICEAAD6AAAAgOgAAHUwAADqYAAAOpgAABdwnLpRPAAAAHBJREFU" +
        "OE+1UksSgCAI5fBNR7NjEUqMlA/FRS4cF+8r0HESE+XvPbToPtqcOJrCHIQwD2aA7jDW6K6iyxw6" +
        "VNzoFjmoqoqtHTzimxA7GAP2AR3KVcD3ap+og+fUtxvUD3PwTdAcltvR4m3s6btDmnkDueGOYn8q" +
        "Ph0AAAAASUVORK5CYII",
    python:
        "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABKUlEQVQ4jZWT642EMAyEv6DrA1MJ" +
        "6QQ6IalkuUoIlZy3ktwPm2V5nFY3EooVeTxjOwTeIEL1E1VQJfABzVtchwHaFvoeYrS7/xQAJmKE" +
        "nCuqn6i3BTKqME0BEWvFXfzpJAA1xpflA0oFeXpc7mcSYqS60qaItlAiqAAJktNSeonuLWgL9PZp" +
        "C/MAs4COIHpUu3P5pWpqG3T05ARl9BOI65UM0LAStINHB21nbcgMJQGrtbE5udtMA7AAaYHnjxG0" +
        "s+HJA2Kx2B/WBWHyFeVTr2WBcd7J82z5J34NAD3UFYIIVQabw/i923byVuD1JkSOFSsLECHlY8/b" +
        "v1EKjON+r3oqEBebwaZ+B1VbN0DJpwLycDXxyRePxTOyuXhDuAyFyciqwHX3l6f8C6FNfiz73B/d" +
        "AAAAAElFTkSuQmCC",
    rss:
        "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" +
        "RU5ErkJggg",
    secure:
        "iVBORw0KGgoAAAANSUhEUgAAAA4AAAAPCAYAAADUFP50AAAACXBIWXMAAA7EAAAOxAGVKw4bAAAB" +
        "y0lEQVQokZ2RvWuTURTGf/fNm4SaNw5JcRMdFAQnwSEiFBdB6CCIq7g4113q1P4Duro4uIkggkOg" +
        "iwU/+kGtDlELxmrExHyUJLRJ3tz3nuPQJE1aXTzLOffhPPc5Dw/8Z5nDwNLyqvZsRO13nXjCJ0in" +
        "MapcvzZzZHdUL/LLWixVtFJv6V6np9ulitZ22vr1R0UfPXmuf1VcerWiqSCg2Wyxtv4BL2ZQUax1" +
        "5HIXODGdZfNjgTu3bhgAf0gsfCly/twZ3rxdZ2F+buKs+YUHemXmEvWd1gjzhoO1jpW1zSMkgMX7" +
        "d8271ff43mj9QLFWbdALe//0/227RDaTmfT48uFxdbaNCIiADrq4/e7GMLyA24u7xgdwts3sXB7P" +
        "vzj4SwEB3NgsiPvM43tXD04VAVwZ/ArSfw1RGRVBESDAP5YDLwvRL8QxSRRbxEueBfmJShmJLBJ2" +
        "UYmIJQMMp+m3N1AZI6qAC7fwU5fRfp2oU8KFIeIcmDiJ/hZMxQibn3BySLFT/U4iI3hTp4gnU8Q1" +
        "ArWgEWgIUsP1Gvu2RkQHzrZA9zD+SYxOA12gi2oHtAtaJQobkx5NLE3+WQF9enMUyTAOHcQxfBML" +
        "gF3+APKIBLDNSR1EAAAAAElFTkSuQmCC",
    sound:
        "iVBORw0KGgoAAAANSUhEUgAAAA8AAAAPCAYAAAA71pVKAAAACXBIWXMAAA7DAAAOwwHHb6hkAAAA" +
        "tUlEQVR4nI3SwQ3DMAgFUEZjFEb5o3CwOgfnTJN7D78HFxK7thohFEfKg4AslDHMQFWtNFPO32QI" +
        "haIK9hxRT2wLiCoIBCPiBlEQ6GmGnwKiCvJ9FiTJiCjg3ujelt1FFdUV6Gf39sVa71sMoPA9/+Lc" +
        "sJkOBRJmbnEv0BeUs7u3aYS+sNdx1nPAAAolnLsucRbI7e4gEGt8/f51QXYQiN3FG2MFh4Xt0Arm" +
        "3I86r+AjPHe/xwcEjQbu0iXJdgAAAABJRU5ErkJggg",
    txt:
        "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAByElEQVQ4jaWTv2tTURTHP+++5JH3" +
        "wnuStEhtEESjvElcLTaYIG7i4Og/EHAJFO3qkkkcHd0cBV0bxAylg0RaUtEpaa2KBkLqC7Fp8+se" +
        "B82jpSFE/MIZzuHcz/1w4RpMSbu5I1prtNZ0Wp9JLl7FducwTROllAFgjJe11lJ+fpezl0a8efmd" +
        "+UWXa0su5bcLZHMNXr+6yLn5gJv3H5NKpbBt2wBQY8DmZoNSbYUXa48AWLh8j7VSmivXC3ysplm6" +
        "fQeASqVCEASndVdXy7K+sSfFfFYanbYU81lpHnSkmM9Kt9+TYj4rWmspFApSr9dlIqD0fksanXZY" +
        "zYOOBIdd6fZ7MhgOQ0CtVgsB6jjEUta0N52YyPFGyx/wsydPw9lyJsONzPJsAP6KPXi4AoCpFFFl" +
        "zm4g5vCUwdjiVi43g8HInGhgmSYjrTGVmg4wRpH/M1AR458NTkwGehAabL+rTLxxqkFf90MDZRgg" +
        "8KFa5eiox49vX/nZ2sf3faLRaHgm/ExBEMjWdp2dvU982d1lv9VCD4c4joPneXieRzKZJJ1O4/s+" +
        "iUTCOGEQj8e5cD7J4a85ztg2ruvieR6O42DbNpZlEYvFwn6c350SyCzTnE/5AAAAAElFTkSuQmCC",
    warn:
        "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAABdFBMVEX/AAAaHWovLmExMGAyMF8y" +
        "MGAxL2CmpraLi6tQR1HvwADgtADitgD8ywDlugDfswDasQDozWdYTk71yAD/4Az/1QG4nU8XBWNN" +
        "Ohj/2QH/1gH/2QTrwADq0V8tMof/7hv/1wB5XSEAABkkGRH/1gD/1QD/4xDmwACeiSn91AD/8hn/" +
        "2gD/7AD/2wD/2QD/4wn30ALqxwUuNIj/+R3/3AAdEEP/5QD/3gD/3QD/7RHoyQDd3OGWiS351wD/" +
        "9xb/4QD/+QAAAGr//gD/4wD/6gjy1QDr0g8kKoT/7wD//x7/5AD/9QBpW7r/6AD/5gD/9RDmzgDX" +
        "1tyflCn/5wD/9wzjzAI3IWi0owH45QTp1AQcH4T/9wD//yRtVSIVAD4QCgbo2ADR0NmyqCD/8wBc" +
        "TS4KADAAAAb/8APp3QATF37//yPDuAL//xPp4ADf3t/Kyda/uhj++AD//wn59ATp4wAREnr//2jr" +
        "6QDg39XKySP//wDz8wAmqcQtAAAAAXRSTlMAQObYZgAAAL5JREFUGJVjYACD6qpKBmRQUV5WVYos" +
        "UFJcVFiQj+Dn5eZkZ1VlIgQy0tNSU5KTEuECCTnxcbEx0VEwfmREenhYaEhwUCBUIMDfz9fH28vT" +
        "wx3Cd3N1cXZydLC3s7WxBgtYWVqYm5n6WpgY+xqB+IYG+nq6Ot6+2npamhogJepqqiraykqKCqqq" +
        "8vZyDAyyMtJSUpIS4mIKUqIiwkIMDIIC/EDAx8sDJLm5ODkYGNjZWFlZWVhYgCQzEyMDBgAArlAi" +
        "6Kl9YD8AAAAASUVORK5CYII",
    xml:
        "iVBORw0KGgoAAAANSUhEUgAAACQAAAAOCAIAAADIVSmfAAAAAXNSR0IArs4c6QAAAARnQU1BAACx" +
        "jwv8YQUAAAAgY0hSTQAAeiYAAICEAAD6AAAAgOgAAHUwAADqYAAAOpgAABdwnLpRPAAAAAlwSFlz" +
        "AAAOxAAADsQBlSsOGwAAAWVJREFUOE/FVCFPw0AYPTwCie0/oBLJ/gFLEAjExOTEMkUI7jKJIghC" +
        "5jZRgVgyJIakZskQJJ0gmYHkI5moPHm8777brVta1VAuJ+7rNe99772vPbCUqe939ffrJ9HKPrZt" +
        "VzWwb07U/5IlA4tljO0fsdy7FpdfGZcvms9Yw9g7MR/7J+HNz7TKpAplyxQQ+XzGBDnhnPXjxYUK" +
        "ZPQ6NkDH7Wal574ts0yzy/JQKshuI2tylgdB1q5GA2AxuihzV6texCUMcKsGGXAf2oKCTgFEV65Z" +
        "RwZZXrcx5s3bWI9MogLZmrKOCy+QTXTgWHSj2soQBvxB485Gmt7vkdF1LOJYUF0bP2YSFeZCsGjY" +
        "KiqDq4gQTDwLe2RryhPNhmPDnsIXXDYgbvS3UU05J5hpen70aaJ9hAK0Syalj+BZm852MsunEVi+" +
        "awcnJTu2Oe+QOYlyC2g5h118s/E/yNOZOj1uYkeH6hecl/8NHJBeKAAAAABJRU5ErkJggg",
    xpi:
        "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAAK/INwWK6QAAABl0RVh0" +
        "U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAAHWSURBVHjaYvz//z8DJQAggJiQOe/fv2fv" +
        "7Oz8rays/N+VkfG/iYnJfyD/1+rVq7ffu3dPFpsBAAHEAHIBCJ85c8bN2Nj4vwsDw/8zQLwKiO8C" +
        "cRoQu0DxqlWrdsHUwzBAAIGJmTNnPgYa9j8UqhFElwPxf2MIDeIrKSn9FwSJoRkAEEAM0DD4DzMA" +
        "yPi/G+QKY4hh5WAXGf8PDQ0FGwJ22d27CjADAAIIrLmjo+MXA9R2kAHvGBA2wwx6B8W7od6CeQcg" +
        "gKCmCEL8bgwxYCbUIGTDVkHDBia+CuotgACCueD3TDQN75D4xmAvCoK9ARMHBzAw0AECiBHkAlC0" +
        "Mdy7x9ABNA3obAZXIAa6iKEcGlMVQHwWyjYuL2d4v2cPg8vZswx7gHyAAAK7AOif7SAbOqCmn4Ha" +
        "3AHFsIDtgPq/vLz8P4MSkJ2W9h8ggBjevXvHDo4FQUQg/kdypqCg4H8lUIACnQ/SOBMYI8bAsAJF" +
        "Pcj1AAEEjwVQqLpAbXmH5BJjqI0gi9DTAAgDBBCcAVLkgmQ7yKCZxpCQxqUZhAECCJ4XgMl493ug" +
        "21ZD+aDAXH0WLM4A9MZPXJkJIIAwTAR5pQMalaCABQUULttBGCCAGCnNzgABBgAMJ5THwGvJLAAA" +
        "AABJRU5ErkJggg",
    zip:
        "iVBORw0KGgoAAAANSUhEUgAAABQAAAAQCAYAAAAWGF8bAAAAxUlEQVQ4jc2TsQ2DMBRELxGDZAdT" +
        "IKo/TdjEO3xGoaKk4A/DBJciItjGKDiiyDWWz9bzs2QDF+eWFqpg2nXdft+pqILLspBUeu9JKkll" +
        "7pCvhqqgc08AwDA80LYtRATz/O7M+iwgta/CyQYc0DRN1K3jFoOZAegZilXYxQAA0zRF84Ntu9xz" +
        "5XrdcRzzEAPgCoAiAu89RCRecAGsxHA1+ximZr8YhuPO7BLDE7A80BLDAhhw8LBLUtd9xEn/6Okv" +
        "diT2/3kBn8BaKKL8+TkAAAAASUVORK5CYII",
};

function css([selectors, icon]) {
    return selectors.map(function (s) "a" + s + ":hover:after").join(", ")
        + " { display: inline-block; width: 0px; vertical-align: middle; margin: -5px 0; content: url(data:image/png;base64," + icons[icon] + ") }";
}

const PROTOCOLS = [
    ["aim", "aim"],
    ["javascript", "warn"],
    ["mailto", "email"],
];

const EXTENSIONS = [
    ["xls xlt", "excel"],
    ["java", "java"],
    ["js", "javascript"],
    ["avi mov mpe mpeg mpg wmv", "movie"],
    ["msi", "msi"],
    ["pdf", "pdf"],
    ["pps ppt", "powerpoint"],
    ["py", "python"],
    ["rss", "rss"],
    ["au mid mp3 ra snd wav wma", "sound"],
    ["txt rtf", "txt"],
    ["exe", "warn"],
    ["doc dot", "msword"],
    ["xpi", "xpi"],
    ["7z bz2 cab gz gzip lzh rar sit tar tgz z zip", "zip"],
];

let style = [];
style.push(EXTENSIONS.map(function ([ext, icon])
    [ext.split(" ").map(function (e) <>[href$=".{e}"]</>), icon]));
style.push(PROTOCOLS.map(function ([ext, icon])
    [ext.split(" ").map(function (e) <>[href^="{e}:"]</>), icon]));

style = util.Array.flatten(style);
style.push([['[href^="#"]'], "internal"]);
style.push([['[onClick]', '[onclick]'], "hand"]);
style.push([[<>[target=_{k}]</> for each (k in ["SELF", "PARENT", "TOP", "CONTENT"])],
            "new"]);
style.push([["[rel=nofollow]"], "nofollow"]);

style = style.map(css).join("\n");
let styleSecure = css([[':not([href^="https:"])'], "insecure"]);
let styleInsecure = css([['[href^="https:"]'], "secure"]);

storage.styles.addSheet(true, "link-targets", "*", style);
storage.styles.addSheet(true, "link-targets-secure", "https:*", styleSecure);
storage.styles.addSheet(true, "link-targets-insecure", "http:*", styleInsecure);

})();
