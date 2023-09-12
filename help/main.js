{
    const Android = window.Android;
    const shadowColor = Android && Android.isDarkTheme() ? '#FFFFFF80' : '#00000080';

    const addStyleSheet = (oldStyle, rules) => {
        const newStyle = document.createElement('style');
        document.head.appendChild(newStyle);
        const sheet = newStyle.sheet;

        let rule, selector, props, prop;

        for (let i = 0; i < rules.length; i++) {
            rule = rules[i];
            selector = rule[0];
            props = '';

            for (let i = 1; i < rule.length; i++) {
                prop = rule[i];
                props += prop[0] + ': ' + prop[1] + ';';
            }

            sheet.insertRule(selector + ' {' + props + '}', sheet.cssRules.length);
        }

        if (oldStyle) {
            document.head.removeChild(oldStyle);
        }

        return newStyle;
    }

    const getHeaderHeight = () => Math.min(screen.availHeight / 4.5, screen.availWidth / 3);

    const mixColors = (base, other, percentage) => {
        const red = parseInt(base.substring(1, 3), 16);
        const green = parseInt(base.substring(3, 5), 16);
        const blue = parseInt(base.substring(5, 7), 16);

        const mixAmount = percentage / 100;

        const mixedRed = Math.round((1 - mixAmount) * red + mixAmount * other);
        const mixedGreen = Math.round((1 - mixAmount) * green + mixAmount * other);
        const mixedBlue = Math.round((1 - mixAmount) * blue + mixAmount * other);

        return `#${mixedRed.toString(16)}${mixedGreen.toString(16)}${mixedBlue.toString(16)}`;
    };

    addStyleSheet(null, [
        ['body',
            ['color', Android ? Android.getTextColor() : ''],
            ['background-color', Android ? Android.getBgColor() : '']
        ],

        ['a',
            ['color', Android ? Android.getThemeColor() : '']
        ],

        ['div.card',
            ['box-shadow', '0 2px 12px 0 ' + shadowColor]
        ],

        ['img, video',
            ['box-shadow', '0 2px 10px 0 ' + shadowColor]
        ],

        ['body *',
            ['scroll-margin-top', getHeaderHeight() / 3 + 5 + 'px']
        ],

        ['span.code',
            ['background-color', Android ? mixColors(Android.getBgColor(), Android.isDarkTheme() ? 255 : 0, Android.isDarkTheme() ? 15 : 10) : '']
        ]
    ]);

    let headerImg, smallHeader;

    const setHeaderHeight = () => {
        if (!headerImg) {
            return;
        }

        const headerHeight = getHeaderHeight();
        const smallHdr = document.scrollingElement.scrollTop > headerHeight * 2 / 3;

        if (smallHeader === smallHdr) {
            return;
        }

        smallHeader = smallHdr;
        headerImg.style.maxHeight = (headerHeight / (smallHdr ? 3 : 1)) + 'px';
        headerImg.src = smallHdr ? '../en/img/icon.webp' : '../en/img/banner.webp';
    }

    document.body.onscroll = () => setHeaderHeight();

    let dynStyle;
    const HDR_COLOR = '#007F7F';
    const ALPHA5 = '80';

    const updateDynStyle = () => {
        const isLandscape = screen.availWidth > screen.availHeight;
        const contentMargin = isLandscape ? Math.max((screen.availWidth - screen.availHeight) / 2.5, 12) : 12;

        // Subtract side margin and card padding.
        const cardWidth = screen.availWidth - 2 * (contentMargin + 15);

        const headerHeight = getHeaderHeight();
        const hdrAlpha = isLandscape && screen.availWidth >= headerHeight * 2 ? ALPHA5 : 'FF';

        dynStyle = addStyleSheet(dynStyle, [
            ['div.content',
                ['margin-left', contentMargin + 'px'],
                ['margin-right', contentMargin + 'px']
            ],

            // Subtract img margin from card width.
            ['img, video',
                ['max-width', Math.min(cardWidth - 10, 500) + 'px'],
            ],

            ['img.medium, video.medium',
                ['max-width', Math.min(cardWidth / 1.25, 400) + 'px'],
            ],

            ['img.small, video.small',
                ['max-width', Math.min(cardWidth / 1.5, 350) + 'px'],
            ],

            ['div.header',
                ['background-image', 'linear-gradient(to right, ' + HDR_COLOR + hdrAlpha + ', ' + HDR_COLOR + ' 20% 80%, ' + HDR_COLOR + hdrAlpha + ' 100%)']
            ],

            ['div.content',
                ['margin-top', headerHeight + 20 + 'px']
            ],

            ['ul, ol',
                ['padding-left', (Math.min(Math.max(cardWidth * 5 / 100, 15), 40)) + 'px']
            ]
        ]);

        setHeaderHeight();
    }

    updateDynStyle();

    screen.orientation.addEventListener("change", function () {
        updateDynStyle();
    });

    // let timeoutId;

    // window.addEventListener('resize', () => {
    //     clearTimeout(timeoutId);
    //     timeoutId = setTimeout(() => updateDynStyle(), 250);
    // });

    class CustomDiv extends HTMLDivElement {
        connectedCallback() {
            if (this.connected_) {
                return;
            }
            this.connected_ = true;

            const type = this.getAttribute('data-custom-type');
            const that = this;

            if (type === 'header-img') {
                setTimeout(() => {
                    headerImg = that.firstElementChild;
                    setHeaderHeight();
                })
            } else if (type === 'langs-cont') {
                // https://www.w3schools.com/tags/ref_language_codes.asp
                setTimeout(() => that.querySelector('#lang-' + document.documentElement.lang).style.display = 'none')
            }
        }
    }

    customElements.define('custom-div', CustomDiv, { extends: 'div' });
}
