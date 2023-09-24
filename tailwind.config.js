module.exports = {
  theme: {
    extend: {
      scale: {
        '30': '0.3',
      },

      animation: {
        finiteSmPing: 'smallPing 1s cubic-bezier(0,0,.2,1) 2',
        infiniteSmPing: 'smallPing 1s cubic-bezier(0,0,.2,1) infinite',
        longCountSmPing: 'smallPing 1s cubic-bezier(0,0,.2,1) 2',
      },
      keyframes: {
        smallPing: {
          '75%, 100%': { transform: 'scale(1.1)', opacity: '0' }
        }
      }
    }
  },
  variants: []
}