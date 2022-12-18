use std::collections::HashMap;

type node_id = String;
type relationship_map = HashMap<node_id, HashMap<node_id, ()>>;

#[derive(Clone)]
pub struct Graph {
    nodes: HashMap<node_id, ()>,

    // The relationship map is a map of node ids to a map of node ids to a
    // unit. The outer map is the source node, and the inner map is the
    // destination nodes.
    dependencies: relationship_map,
    dependents: relationship_map,
}

impl Graph {
    pub fn new() -> Graph {
        Graph {
            nodes: HashMap::new(),
            dependencies: HashMap::new(),
            dependents: HashMap::new(),
        }
    }

    pub fn add_node(&mut self, node: node_id) {
        self.nodes.insert(node, ());
    }

    pub fn dependencies(&self) -> HashMap<node_id, ()> {
        // Flatten the dependencies map into a single map of node ids to a unit.
        HashMap::from_iter(
            self.dependencies
                .iter()
                .flat_map(|(source, dests)| dests.iter().map(move |(dest, _)| (dest.clone(), ()))),
        )
    }

    pub fn depends_on(&self, child: node_id, parent: node_id) -> bool {
        self.dependencies
            .get(&child)
            .map(|map| map.contains_key(&parent))
            .unwrap_or(false)
    }

    pub fn add_dependency(&mut self, child: node_id, parent: node_id) {
        if child == parent {
            return;
        }

        self.add_node(child.clone());
        self.add_node(parent.clone());

        self.dependencies
            .entry(child.clone())
            .or_insert(HashMap::new())
            .insert(parent.clone(), ());
        self.dependents
            .entry(parent.clone())
            .or_insert(HashMap::new())
            .insert(child.clone(), ());
    }

    pub fn leaves(&self) -> Vec<node_id> {
        self.nodes
            .iter()
            .filter(|(node, _)| !self.dependents.contains_key(*node))
            .map(|(node, _)| node.clone())
            .collect()
    }

    pub fn remove(&mut self, node: node_id) {
        self.nodes.remove(&node);

        if let Some(dependents) = self.dependents.remove(&node) {
            for dependent in dependents.keys() {
                self.dependencies.get_mut(dependent).unwrap().remove(&node);
            }
        }

        if let Some(dependencies) = self.dependencies.remove(&node) {
            for dependency in dependencies.keys() {
                self.dependents.get_mut(dependency).unwrap().remove(&node);
            }
        }

        let keys = self.dependents.keys().cloned().collect::<Vec<_>>();
        for k in keys {
            let dependents = self.dependents.get_mut(&k).unwrap();
            if dependents.len() == 0 {
                self.dependents.remove(&k);
            }
        }
    }

    pub fn topo_sort(&self) -> Vec<node_id> {
        let mut graph = self.clone();
        let mut sorted = Vec::new();

        while !graph.nodes.is_empty() {
            let leaves = graph.leaves();

            if leaves.is_empty() {
                break;
            }

            for leaf in leaves {
                sorted.push(leaf.clone());
                graph.remove(leaf);
            }
        }

        for node in graph.nodes.keys() {
            sorted.push(node.clone());
        }

        sorted
    }
}
