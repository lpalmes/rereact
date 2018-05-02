open RereactTypes;

let updateQueue: ref(list(fiberUpdate)) = ref([]);

let nextUnitOfWork: ref(option(opaqueFiber)) = ref(None);

let fiberRoot: ref(option(opaqueFiber)) = ref(None);

let pendingCommit: ref(option(opaqueFiber)) = ref(None);

type worker = {mutable work: unit => unit};

let globalWorker = {work: () => ()};