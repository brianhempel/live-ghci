bin/live-ghci: LiveGhci.hs
	ghc -threaded -O LiveGhci.hs
	mkdir -p bin; cp LiveGhci bin/live-ghci

# Auto-rebuild on file save. Requires fswatch (install with your package manager).
watch:
	fswatch -l 0.1 -o *.hs | xargs -n1 -I{} make
